#	PioneerAvpControl
#
#	Author:	Werner Lindbuechl
#	Credit To: Chris Couper <ccouper(at)fastkat(dot)com>
#	Credit To: Felix Mueller <felix(dot)mueller(at)gwendesign(dot)com>
#
#	Copyright (c) 2012 Werner Lindbuechl
#	All rights reserved.
#
#	----------------------------------------------------------------------
#	Function:	Turn Pioneer AVP Amplifier on and off (works for TP and SB)
#	----------------------------------------------------------------------
#	Technical:
#
#	----------------------------------------------------------------------
#	Installation:
#			- Copy the complete directory into the 'Plugins' directory
#			- Restart SlimServer
#			- Enable PioneerAvpControl in the Web GUI interface
#			- Set:AvpIP Address, On and Off Delays, Max Volume, Input
#	----------------------------------------------------------------------
#	History:
#
#	2012/11/22 v1.0	- Initial version
#	----------------------------------------------------------------------
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#	02111-1307 USA
#
package Plugins::PioneerAvpControl::Plugin;

use Errno qw(EAGAIN);
use Errno qw(EINTR);

use strict;
use base qw(Slim::Plugin::Base);

use Slim::Utils::Strings qw(string);
use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Misc;

use IO::Socket::INET;

use Plugins::PioneerAvpControl::Settings;

#use Data::Dumper; #used to debug array contents

# ----------------------------------------------------------------------------
# Global variables
# ----------------------------------------------------------------------------
my @AV_POW_GET = qw(?P ?AP ?BP);
my @AV_POW_SET = qw(P AP BP);
my @AV_POW_RES = qw(PWR(.) APR(.) BPR(.));
my @AV_POW_VAL = qw(F O);

my @AV_VOL_GET = qw(?V ?ZV ?YV);
my @AV_VOL_SET = qw(VL ZV YV);
my @AV_VOL_RES = qw(VOL(...) ZV(..) YV(..));

my @AV_INP_GET = qw(?F ?ZS ?ZT);
my @AV_INP_SET = qw(FN ZS ZT);
my @AV_INP_RES = qw(FN(..) Z2F(..) Z3F(..));

my @AV_POW_DELAY_PARAM = qw(delayOff delayOn);

my $CR = "\r";
my $CRLF = "\r\n";

my %avStateMap;
my $getexternalvolumeinfoCoderef; #used to report use of external volume control

# ----------------------------------------------------------------------------
# References to other classes
# my $classPlugin = undef;

# ----------------------------------------------------------------------------
my $log = Slim::Utils::Log->addLogCategory({
	'category'     => 'plugin.pioneeravpcontrol',
	'defaultLevel' => 'INFO',
	'description'  => 'PLUGIN_PIONEERAVPCONTROL_MODULE_NAME',
});

# ----------------------------------------------------------------------------
my $prefs = preferences('plugin.pioneeravpcontrol');

# ----------------------------------------------------------------------------
sub initPlugin {
	my $classPlugin = shift;

	# Not Calling our parent class prevents adds it to the player UI for the audio options
	$classPlugin->SUPER::initPlugin();

	# Initialize settings classes
	my $classSettings = Plugins::PioneerAvpControl::Settings->new( $classPlugin);

	# Install callback to get client setup
	Slim::Control::Request::subscribe( \&newPlayerCheck, [['client']],[['new']]);

	# getexternalvolumeinfo
	$getexternalvolumeinfoCoderef = Slim::Control::Request::addDispatch(['getexternalvolumeinfo'],[0, 0, 0, \&getexternalvolumeinfoCLI]);
	$log->info( "getexternalvolumeinfoCoderef: ".$getexternalvolumeinfoCoderef);
}

#print unpack('H*',$regex)."\n";
#$log->debug("HK: pow=$avStateMap{'HK'}->{POW} inp=$avStateMap{'HK'}->{INP} vol=$avStateMap{'HK'}->{VOL}");
# ----------------------------------------------------------------------------
sub newPlayerCheck {
	my $request = shift;
	my $client = $request->client();

    if (defined($client)) {
		# Do nothing if client is not a Receiver or Squeezebox
		if(!(($client->isa("Slim::Player::Receiver")) || ($client->isa("Slim::Player::Squeezebox2")))) {
			$log->debug( "not a receiver or a squeezebox b");
			#now clear callback for those clients that are not part of the plugin
			clearCallback();
			return;
		}

		#init the client
		my $cprefs = $prefs->client($client);
		my $avpIPAddress = $cprefs->get('avpAddress');
		my $inp = $cprefs->get('input');
		my $zone = $cprefs->get('zone');
		my $pluginEnabled = $cprefs->get('pref_Enabled');
		my $audioEnabled = $cprefs->get('pref_AudioMenu');

		# Do nothing if plugin is disabled for this client
		if (!defined( $pluginEnabled) || $pluginEnabled == 0) {
			$log->debug("[".$client->name()."] plugin not enabled {".$client."}");
			#now clear callback for those clients that are not part of the plugin
			clearCallback();
			return;
		} else {
			$log->debug("[".$client->name()."] plugin enabled (input=$inp zone=$zone ip=$avpIPAddress) {".$client."}");

			# Install callback to get client state changes
			Slim::Control::Request::subscribe( \&callback, [['power', 'play', 'pause', 'playlist', 'client', 'mixer']], $client);			
		}
	}
}

# ----------------------------------------------------------------------------
sub getDisplayName {
	return 'PLUGIN_PIONEERAVPCONTROL';
}

# ----------------------------------------------------------------------------
sub shutdownPlugin {
	Slim::Control::Request::unsubscribe(\&newPlayerCheck);
	clearCallback();
	
	# Give up rerouting
	Slim::Control::Request::addDispatch( ['getexternalvolumeinfo'], [0, 0, 0, $getexternalvolumeinfoCoderef]);	
}

# ----------------------------------------------------------------------------
sub clearCallback {
	Slim::Control::Request::unsubscribe(\&callback);
	# Give up rerouting
}

# ----------------------------------------------------------------------------
# Callback to get client state changes
# ----------------------------------------------------------------------------
sub callback {
	my $request = shift;
	my $client = $request->client();

#	if(!defined($client)) {
		# unsubscribe?
#		return;
#	}
	my $cprefs = $prefs->client($client);

	$log->debug("[".$client->name()."] *** SB Event p0:[".$request->{'_request'}[0]."] p1:[".$request->{'_request'}[1]."]");
	
	my $param = shift;

	if($request->isCommand([['power']])) {
		my $sbPow = $client->power;
		my $delay = $cprefs->get($AV_POW_DELAY_PARAM[$sbPow]);
		
		Slim::Utils::Timers::killTimers($client, \&handleSBPowerEvent); 
 		Slim::Utils::Timers::setTimer($client, (Time::HiRes::time() + $delay), \&handleSBPowerEvent, $sbPow);		

	} elsif($request->isCommand([['mixer'], ['volume']])) {
		my $avInitiated = $request->getResult('avInitiated');
		if(!$avInitiated) {
			my $sbVol = $client->volume();

			Slim::Utils::Timers::killTimers( $client, \&handleSBVolumeEvent);
			Slim::Utils::Timers::setTimer($client, (Time::HiRes::time() + .125), \&handleSBVolumeEvent, $sbVol);		
		}
		
	} elsif($request->isCommand([['playlist'], ['newsong']])) {
		my $volSync = $cprefs->get('pref_VolSynch');
		if ($volSync == 1) {
			handleSBTrackChangeEvent($client);
		}
	}
}

# ----------------------------------------------------------------------------
sub handleSBPowerEvent {
	my $client = shift;
	my $sbPow = shift;

	my $cprefs = $prefs->client($client);
	my $ip = $cprefs->get('avpAddress');
	my $zone = $cprefs->get('zone');

	$log->info("[".$client->name."] zone='$zone' sbPow='$sbPow'");
	
	# request power state
	my $cmd = $AV_POW_GET[$zone];
	my $response = send2AV($ip, $cmd);
	my $avPow = match($response, $AV_POW_RES[$zone]);
	$avPow = $avPow eq "0" ? 1 : 0;
	
	if($sbPow != $avPow) { # power state different?
		my $avInp;
		my $avVol;

		my $timeout;
		if($sbPow == 1) { # send power on?
			$timeout = 1;
		}

		# send power on/off
		$cmd = $AV_POW_SET[$zone].$AV_POW_VAL[$sbPow];
		$response = send2AV($ip, $cmd, $timeout);
		$avPow = match($response, $AV_POW_RES[$zone]); # match power => 0=On/1=Off
		$avPow = $avPow eq 0 ? 1 : 0; # negate => 1=On/0=Off

		if($sbPow == 1) { # send power on?
			$avInp = match($response, $AV_INP_RES[$zone]); # match input
			$avVol = match($response, $AV_VOL_RES[$zone]); # match volume

			my $inp = $cprefs->get('input');
			if($inp ne "99" && $inp ne $avInp) { # prefered input and input different?
				$cmd = $inp.$AV_INP_SET[$zone];
				$response = send2AV($ip, $cmd);
				$avInp = match($response, $AV_INP_RES[$zone]); # match input
			}
		}
		putAVState($client, $avPow, $avInp, $avVol);
	}
}

# ----------------------------------------------------------------------------
sub handleSBVolumeEvent {
	my $client = shift;
	my $sbVol = shift;

	my $cprefs = $prefs->client($client);
	my $ip = $cprefs->get('avpAddress');
	my $zone = $cprefs->get('zone');
	my $maxVol = $cprefs->get('maxVol');

	my $avVol;	
	if($zone == 0) {
		$avVol = sprintf("%03d", (((80 + $maxVol) / 0.5) + 1) * ($sbVol / 100));
	} else {
		$avVol = sprintf("%02d", (80 + $maxVol + 1) * ($sbVol / 100));
	}

	my $cmd = $avVol.$AV_VOL_SET[$zone];
	my $response = send2AV($ip, $cmd, 1);
	my $avVolRead = match($response, $AV_VOL_RES[$zone]);
	$log->info("[".$client->name."] zone='$zone' sbVol='$sbVol' => avVol='$avVol' avVolRead='$avVolRead'");
	
	putAVState($client, undef, undef, $avVolRead);
}

# ----------------------------------------------------------------------------
sub handleSBTrackChangeEvent {
	my $client = shift;

	my $cprefs = $prefs->client($client);
	my $ip = $cprefs->get('avpAddress');
	my $zone = $cprefs->get('zone');
	my $maxVol = $cprefs->get('maxVol');
	
	my $cmd = $AV_VOL_GET[$zone];
	my $response = send2AV($ip, $cmd);
	my $avVol = match($response, $AV_VOL_RES[$zone]);
	
	my $sbVol = sprintf("%d", ($avVol * 100) / (((80 + $maxVol) / 0.5) + 1));
	$log->info("[".$client->name."] zone='$zone' avVol='$avVol' => sbVol='$sbVol'");

	my $request = $client->execute([('mixer', 'volume', $sbVol)]);
	$request->addResult('avInitiated', 1); # prevent feedback loop
	
	putAVState($client, undef, undef, $avVol);
}

sub putAVState {
	my $client = shift;
	my $avPow = shift;
	my $avInp = shift;
	my $avVol = shift;
	
	#$log->debug("[".$client->name."] avPow='$avPow' avInp='$avInp' avVol='$avVol'");
	my $avState = $avStateMap{$client};
	if(!$avState) {
		$avState = {
			POW => $avPow,
			INP => $avInp,
			VOL => $avVol,
		};
		$avStateMap{$client} = $avState;
	} else {
		if(defined($avPow)) { $avState->{POW} = $avPow; }
		if(defined($avInp)) { $avState->{INP} = $avInp; }
		if(defined($avVol)) { $avState->{VOL} = $avVol; }
	}
	$log->debug("[".$client->name."] pow='".$avState->{POW}."' inp='".$avState->{INP}."' vol='".$avState->{VOL}."'");
}

sub send2AV {
	my $ip = shift;
	my $cmd = shift;
	my $timeout = shift;
	
	if (!$timeout) {
		$timeout = .125;
	}
	
	my $socket = IO::Socket::INET->new(
		PeerHost => $ip,
		PeerPort => '23',
		Proto => 'tcp',
		);
	
	if(!$socket) {
		$log->error("Can't connect to '$ip'");
		return '';
	}
	$socket->autoflush(1);
	
	$log->debug("Sending '".subsCrLf($cmd)."'");
	$socket->send($cmd."\r");
	my $data = readFromAv($socket, $timeout);
	$socket->close();

	$log->debug("Receiving '".subsCrLf($data)."' (".unpack('H*', $data).")");
	
	return $data;
}

sub readFromAv {
	my $socket = shift;
	my $timeout = shift;
	
	my $select = IO::Select->new($socket);
	my $buffer = '' ;
	my $rc ;
	while (1) {
		$! = undef ;               # no error, yet
		if ($select->can_read($timeout)) {
			$rc = sysread($socket, $buffer, 64, length($buffer));
			next if $rc ;            # continue if not error and not "eof"
			last if defined($rc) ;   # exit if "eof"
		}
		else {
			$rc = $! ? undef : 1 ;   # $rc == undef if error, == 1 if time out
			last if $rc ;            # exit if time out
		} ;

		redo if $! == EAGAIN ;     # Not really expected, even with non-blocking
		redo if $! == EINTR ;      # Signals !

		last ;                     # exit on "hard" error
	};
	return $buffer;
}

sub match {
	my $response = shift;
	my $pattern = shift;

	my $regex = "[.*\n]*$pattern\r\n";
	$response =~ /$regex/;
	my $value = $1;
	$log->debug("Matched $pattern: '".subsCrLf($value)."'". (defined($value) ? "" : "undefined"));
	return $value;
}

sub subsCrLf {
	my $msg = shift;
	$msg =~ s/\n/\\n/g;
	$msg =~ s/\r/\\r/g;
	
	return $msg;
}


# --------------------------------------- start external volume indication code -------------------------------
# used by iPeng and other controllers
sub getexternalvolumeinfoCLI {
	my @args = @_;
	&reportOnOurPlayers();
	if (defined($getexternalvolumeinfoCoderef)) {
		# chain to the next implementation
		return &$getexternalvolumeinfoCoderef(@args);
	}
	# else we're authoritative
	my $request = $args[0];
	$request->setStatusDone();
}

# ----------------------------------------------------------------------------
sub reportOnOurPlayers() {
	# loop through all currently attached players
	foreach my $client (Slim::Player::Client::clients()) {
		if (&usingPioneerAvpControl($client) ) {
			# using our volume control, report on our capabilities
			$log->info("Note that ".$client->name()." uses us for external volume control");
			Slim::Control::Request::notifyFromArray($client, ['getexternalvolumeinfo', 'relative:1', 'precise:1', string(&getDisplayName())]);			
			# precise:1		can set exact volume
			# relative:1		can make relative volume changes
			# plugin:PioneerSerial	this plugin's name
		}
	}
}

# ----------------------------------------------------------------------------
# determine if this player is using the PioneerAvpControl plugin and its enabled
sub usingPioneerAvpControl() {
	my $client = shift;
	my $cprefs = $prefs->client($client);
	my $pluginEnabled = $cprefs->get('pref_Enabled');

	# cannot use DS if no digital out (as with Baby)
	if ( (!$client->hasDigitalOut()) || ($client->model() eq 'baby')) {
		return 0;
	}
 	if ($pluginEnabled == 1) {
		return 1;
	}
	return 0;
}
	
# --------------------------------------- end external volume indication code -------------------------------

my @avPow = qw(1 1 1);
my @avInp = qw(04 06 07);
my @avVol = qw(000 20 10);

sub send2AVSim {
	my $ip = shift;
	my $cmd = shift;
	
	my $response = '';
	
	$log->debug("Sending: '".subsCrLf($cmd)."'");
	my $zone;
	my $regex;
	for($zone=0;$zone<3;$zone++) {
		# power request
		if($cmd eq $AV_POW_GET[$zone]) { 
			$response .= replResParam($AV_POW_RES[$zone], $avPow[$zone]);
			last;
		}
		
		# power set
		$regex = "^".$AV_POW_SET[$zone]."(.)";
		if($cmd =~ /$regex/) {
			if($1 eq "O") {
				if($avPow[$zone] != 0) {
					$avPow[$zone] = 0;
					$response .= replResParam($AV_POW_RES[$zone], $avPow[$zone]);
					$response .= "LM0401".$CRLF;
					$response .= replResParam($AV_INP_RES[$zone], $avInp[$zone]);
					$response .= "FL202020202502020202".$CRLF;
					$response .= replResParam($AV_VOL_RES[$zone], $avVol[$zone]);
				}
			} else {
				if($avPow[$zone] != 1) {
					$avPow[$zone] = 1;
					$response .= replResParam($AV_POW_RES[$zone], $avPow[$zone]);
				}
			}
			last;
		}
		
		# volume request
		if($cmd eq $AV_VOL_GET[$zone]) {
			$response .= replResParam($AV_VOL_RES[$zone], $avVol[$zone]);
			last;
		}
		
		# volume set
		$regex = "^(.*)".$AV_VOL_SET[$zone];
		if($cmd =~ /$regex/) {
			$avVol[$zone] = $1;
			$response .= replResParam($AV_VOL_RES[$zone], $avVol[$zone]);
			last;
		}
		
		# input set
		$regex = "^(.*)".$AV_INP_SET[$zone];
		if($cmd =~ /$regex/) {
			if($avInp[$zone] != $1) {
				$avInp[$zone] = $1;
				$response .= replResParam($AV_INP_RES[$zone], $avInp[$zone]);
			}
			last;
		}
	}
	my $msg = $response;
	
	$log->debug("Received: '".subsCrLf($response)."'");
	return $response;
}

sub replResParam {
	my $response = shift;
	my $param = shift;
	
	$response =~ s/\(.*\)/$param/;
	return $response.$CRLF;
}

# end with something for plugin to do
1;
