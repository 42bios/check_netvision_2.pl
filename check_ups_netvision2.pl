#! /usr/bin/perl -w
#
# check_netvision - nagios plugin
#
# Copyright (C) 2013 Guenther Mair,
# Derived from check_ifoperstatus by Christoph Kron.
#
#
# Modified by Manuel Mahr manuel (at) it-mahr.com
#
#
#
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#
# Report bugs to:  guenther.mair (at) hoslo.ch
#
# $Id$
#
#
#
#
## 2019-08-09: Manuel Mahr  manuel (at) it-mahr.com
#	- added remaining time
#   - added battery capacity


use POSIX;
use Math::BigInt;
use strict;
use lib "/usr/local/nagios/libexec";
use utils qw($TIMEOUT %ERRORS &print_revision &support);

use Net::SNMP;
use Getopt::Long;
&Getopt::Long::config('bundling');

## function prototypes

sub print_help ();
sub usage ();
sub process_arguments ();
sub in_array ($@);

## module-specific variables

my $PROGNAME = "check_netvision";
my $REVISION = '$Rev$';
my $debug;
my $exclude = '';
my $warnings = '';
my @all_exclude;
my @all_warnings;
my $countComponents = 0;
my $state = 'OK';
my $answer = "";
my $tmp = "";

my $tempBattery;
my $tempEMD;
my $voltage;
my $voltage_min = 200;
my $load;
my $load_max = 95;
my $temp_max = 30;
my $alarms;

my $remTime;
my $time_min = 10;

my $batteryCapacity;
my $batteryCapacity_min = 80;

## variables for argument handling

my $opt_h;
my $opt_V;
my $status;

## snmp specific variables

my $timeout;
my $hostname;
my $session;
my $error;
my $response;
my $key;
my $lastc;
my $name;
my $community = "public";
my $snmp_version = 1;
my $maxmsgsize = 1472; # Net::SNMP default is 1472
my ($seclevel, $authproto, $secname, $authpass, $privpass, $auth, $priv);
my $context = "";
my $port = 161;
my @snmpoids;


## Socomec Netvision specific states

my %upsBatteryStates = (
  1 => "unknown(1)",
  2 => "batteryNormal(2)",
  3 => "batteryLow(3)",
  4 => "batteryDepleted(4)",
  5 => "batteryDischarging(5)",
  6 => "batteryFailure(6)",
  7 => "upsOff(7)"
);

my %upsOutputSources = (
  1 => "unknown(1)",
  2 => "onInverter(2)",
  3 => "onMains(3)",
  4 => "ecoMode(4)",
  5 => "onBypass(5)",
  6 => "standby(6)",
  7 => "onMaintenanceBypass(7)",
  8 => "upsOff(8)",
  9 => "normalMode(9)"
);

## Socomec Netvision system OIDs (ascending numeric order), names and state-types
# 1.3.6.1.4.1.4555.1.1.1.1 (.iso.org.dod.internet.private.enterprises.socomecSicon.software.network.netvision.upsObjects)

my %upsComponents = (
   upsBatteryStatus => {
     OID  => '1.3.6.1.4.1.4555.1.1.1.1.2.1',
     name => 'present battery status',
     type => 'upsBatteryStates',
   });

my $emdSatatusTemperature = '1.3.6.1.4.1.4555.1.1.1.1.11.1.0';    # .emdSatatus.emdSatatusTemperature
my $upsBatteryTemperature = '1.3.6.1.4.1.4555.1.1.1.1.2.6.0';     # .upsBattery.upsBatteryTemperature
my $upsOutputVoltage      = '1.3.6.1.4.1.4555.1.1.1.1.4.4.1.2.1'; # .upsOutput.upsOutputTable.upsOutputEntry.upsOutputVoltage (by factor 10)
my $upsOutputPercentLoad  = '1.3.6.1.4.1.4555.1.1.1.1.4.4.1.4.1'; # .upsOutput.upsOutputTable.upsOutputEntry.upsOutputPercentLoad (output load rate in %)
my $upsOutputSource       = '1.3.6.1.4.1.4555.1.1.1.1.4.1.0';     # .upsOutput.upsOutputSource
my $upsAlarmsPresent      = '1.3.6.1.4.1.4555.1.1.1.1.6.1.0';     # .upsAlarm.upsAlarmsPresent
my $upsRemainingtime	  = '1.3.6.1.4.1.4555.1.1.1.1.2.3.0';	  # .upsRemainingtime (output in min)
my $upsBatteryCapacity	  = '1.3.6.1.4.1.4555.1.1.1.1.2.4.0'; 	  # .upsBatteryCapacity (output in %)
## validate arguments

process_arguments();


## just in case of problems, let's avoid blocking the calling process for too long

$SIG{'ALRM'} = sub {
  print ("ERROR: No snmp response from $hostname (alarm)\n");
  exit $ERRORS{"UNKNOWN"};
};

alarm($timeout);


## main function

print "Socomec Sicon Netvision Check:";


## component checks

for my $component (keys %upsComponents) {
  if (in_array($component, @all_exclude)) {
    print " excluding '".$component."' from check\n" if (defined $debug);
    next;
  }

  fetch_status($upsComponents{$component}{OID}, $upsComponents{$component}{name}, $upsComponents{$component}{type});
}


## single value checks

push(@snmpoids, $emdSatatusTemperature);
push(@snmpoids, $upsBatteryTemperature);
push(@snmpoids, $upsOutputVoltage);
push(@snmpoids, $upsOutputPercentLoad);
push(@snmpoids, $upsOutputSource);
push(@snmpoids, $upsAlarmsPresent);
push(@snmpoids, $upsRemainingtime);
push(@snmpoids, $upsBatteryCapacity);

if ( ! defined($response = $session->get_request(@snmpoids)) ) {
  $answer=$session->error;
  $session->close;
  $state = 'WARNING';
  print "$state: SNMP error: $answer\n";
  exit $ERRORS{$state};
}

# verify remaining time
$remTime = $response->{$upsRemainingtime};
if ($remTime < $time_min) {
  $state = 'CRITICAL';
  print " remaining time under $time_min (currently at $remTime) battery capacity (currently at $batteryCapacity%)  |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify battery temperature
$tempBattery = $response->{$upsBatteryTemperature};
if ($tempBattery > $temp_max) {
  $state = 'CRITICAL';
  print " battery temperature above $temp_max (currently at $tempBattery) |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify EMD temperature
$tempEMD = $response->{$emdSatatusTemperature};
if ($tempEMD > ($temp_max * 10)) {
  $state = 'CRITICAL';
  print " EMD temperature above $temp_max (currently at $tempEMD) |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify voltage
$voltage = Math::BigInt->new($response->{$upsOutputVoltage});
if ($voltage < $voltage_min) {
  $state = 'CRITICAL';
  print " output voltage below $voltage_min (currently at $voltage) |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify load
$load = Math::BigInt->new($response->{$upsOutputPercentLoad});
if ($load > $load_max) {
  $state = 'CRITICAL';
  print " maximum load reached (currently at $load) |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify battery capacity 
$batteryCapacity = Math::BigInt->new($response->{$upsBatteryCapacity});
if ($batteryCapacity < $batteryCapacity_min) {
  $state = 'CRITICAL';
  print " battery capacity is below $batteryCapacity_min% (currently at $batteryCapacity%)  remaining time: $remTime |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

# verify alarms
$alarms = Math::BigInt->new($response->{$upsAlarmsPresent});
if ($alarms > 0) {
  $state = 'CRITICAL';
  print " there are alarms on the UPS - clear them! |load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
}

my $source = $upsOutputSources{$response->{$upsOutputSource}};


# exit
if ( ! defined $debug && $state eq 'OK') {
  print " overall system state OK $source|load=$load tempbattery=$tempBattery remaining_time=$remTime capacity=$batteryCapacity tempemd=$tempEMD voltage=$voltage\n";
} else {
  print " $source\n";
}
exit $ERRORS{$state};


## subroutines

#
# $_[0] component OID
# $_[1] component name
# $_[2] component type
#
sub fetch_status {
  my $value;

  if ( ! defined ($response = $session->get_table($_[0]))) {
    print  " " . $_[1] . " - " . $_[0] . " (OID-tree not found, ignoring)\n" if (defined $debug);
    # tree not found, ignore!
    return -1;
  }

  while (($key, $value) = each %{$response}) {
    if ($value > 2) {
      # 1 = other/unknow  => assume OK
      # 2 = ok            => OK
      # 3 = failure/worse => CRITICAL/WARNING
      if (in_array($key, @all_warnings) && $state ne 'CRITICAL') {
        $state = 'WARNING';
      } else {
        $state = 'CRITICAL';
      }
    }
    if (defined $debug || $value > 2) {
      print " " . $_[1] . " (";
      if (defined $key && (length($key) > (length($_[0])+2))) {
        print substr($key, length($_[0])+1) . ":";
      }
      # eval to something like '$cpqGenericStates{$value}'
      $tmp = eval("\$" . $_[2] . "{" . $value . "}");
      if ($debug) {
        print $tmp."\n" if ($tmp ne "");
      } else {
        print $tmp." " if ($tmp ne "");
      }
      print ")";
    }
    $countComponents++;
    print "\n" if (defined $debug);
  }
}

sub in_array($@) {
   my $needle = shift(@_);
   my %items = map {$_ => 1} @_;
   return (exists($items{$needle})) ? 1 : 0;
}

sub usage() {
  printf "\nMissing arguments!\n";
  printf "\n";
  printf "Usage: \n";
  printf "check_netvision -H <HOSTNAME> [-C <community>] [-d] [-x excludecomponent1,excludecomponent2,...]\n";
  printf "Copyright (C) 2013 Guenther Mair\n";
  printf "\nUse 'check_netvision --help' for details.\n";
  printf "\n\n";
  exit $ERRORS{"UNKNOWN"};
}

sub print_help() {
  printf "check_netvision plugin for Nagios/Icinga\n";
  printf "\nModule specific parameters:\n";
  printf "   -d (--debug)      debug / verbose mode (print checked details)\n";
  printf "   -x (--exclude)    exclude components from check (separate by commas)\n";
  printf "   -w (--warnings)   use warnings instead of criticals for these components (separate by commas)\n";
  printf "   -m (--min_volt)   specify the minimum output voltage (defaults to 200)\n";
  printf "   -l (--max_load)   specify the maximum output load in percent (defaults to 95)\n";
  printf "       --temperature specify the maximum temperature in degrees celcius (defaults to 37)\n";
  printf "\nCurrently the module supports the following components:\n " . join(",\n ", keys %upsComponents) . "\n";
  printf "\nSNMP parameters:\n";
  printf "   -H (--hostname)   Hostname to query (required)\n";
  printf "   -C (--community)  SNMP read community (defaults to public,\n";
  printf "                     used with SNMP v1 and v2c\n";
  printf "   -v (--snmp_version)  1 for SNMP v1 (default)\n";
  printf "                        2 for SNMP v2c\n";
  printf "                        SNMP v2c will use get_bulk for less overhead\n";
  printf "   -L (--seclevel)   choice of \"noAuthNoPriv\", \"authNoPriv\", or \"authPriv\"\n";
  printf "   -U (--secname)    username for SNMPv3 context\n";
  printf "   -A (--authpass)   authentication password (cleartext ascii or localized key\n";
  printf "                     in hex with 0x prefix generated by using \"snmpkey\" utility\n";
  printf "                     auth password and authEngineID\n";
  printf "   -a (--authproto)  Authentication protocol (MD5 or SHA1)\n";
  printf "   -X (--privpass)   privacy password (cleartext ascii or localized key\n";
  printf "                     in hex with 0x prefix generated by using \"snmpkey\" utility\n";
  printf "                     privacy password and authEngineID\n";
  printf "   -p (--port)       SNMP port (default 161)\n";
  printf "   -M (--maxmsgsize) Max message size - usefull only for v1 or v2c\n";
  printf "   -t (--timeout)    seconds before the plugin times out (default=$TIMEOUT)\n";
  printf "   -V (--version)    Plugin version\n";
  printf "   -h (--help)       usage help \n\n";
  print_revision($PROGNAME, '$Revision: 48 $');
}

sub process_arguments() {
  $status = GetOptions(
    "V"   => \$opt_V,        "version"        => \$opt_V,
    "h"   => \$opt_h,        "help"           => \$opt_h,
    "d"   => \$debug,        "debug"          => \$debug,
    "x=s" => \$exclude,      "exclude=s"      => \$exclude,
    "w=s" => \$warnings,     "warnings=s"     => \$warnings,
    "m=i" => \$voltage_min,  "min_volt=i"     => \$voltage_min,
    "l=i" => \$load_max,     "max_load=i"     => \$load_max,
                             "temperature=i"  => \$temp_max,
    "v=i" => \$snmp_version, "snmp_version=i" => \$snmp_version,
    "C=s" => \$community,    "community=s"    => \$community,
    "L=s" => \$seclevel,     "seclevel=s"     => \$seclevel,
    "a=s" => \$authproto,    "authproto=s"    => \$authproto,
    "U=s" => \$secname,      "secname=s"      => \$secname,
    "A=s" => \$authpass,     "authpass=s"     => \$authpass,
    "X=s" => \$privpass,     "privpass=s"     => \$privpass,
    "p=i" => \$port,         "port=i"         => \$port,
    "H=s" => \$hostname,     "hostname=s"     => \$hostname,
    "M=i" => \$maxmsgsize,   "maxmsgsize=i"   => \$maxmsgsize,
    "t=i" => \$timeout,      "timeout=i"      => \$timeout,
  );

  @all_exclude = split(/,/, $exclude);
  @all_warnings = split(/,/, $warnings);

  if ($status == 0) {
    print_help();
    exit $ERRORS{'OK'};
  }

  if ($opt_V) {
    print_revision($PROGNAME,'$Revision: 48 $');
    exit $ERRORS{'OK'};
  }

  if ($opt_h) {
    print_help();
    exit $ERRORS{'OK'};
  }

  if (!utils::is_hostname($hostname)) {
    usage();
    exit $ERRORS{"UNKNOWN"};
  }

  $timeout = $TIMEOUT unless (defined $timeout);

  if ($snmp_version =~ /3/) {
    # Must define a security level even though default is noAuthNoPriv
    # v3 requires a security username
    if (defined $seclevel  && defined $secname) {

      # Must define a security level even though defualt is noAuthNoPriv
      unless ( grep /^$seclevel$/, qw(noAuthNoPriv authNoPriv authPriv) ) {
        usage();
        exit $ERRORS{"UNKNOWN"};
      }

      # Authentication wanted
      if ( $seclevel eq 'authNoPriv' || $seclevel eq 'authPriv' ) {
        unless ( $authproto eq 'MD5' || $authproto eq 'SHA1' ) {
          usage();
          exit $ERRORS{"UNKNOWN"};
        }

        if ( ! defined $authpass) {
          usage();
          exit $ERRORS{"UNKNOWN"};
        } else {
          if ($authpass =~ /^0x/) {
            $auth = "-authkey => $authpass" ;
          } else {
            $auth = "-authpassword => $authpass";
          }
        }
      }

      # Privacy (DES encryption) wanted
      if ($seclevel eq 'authPriv') {
        if ( ! defined $privpass) {
          usage();
          exit $ERRORS{"UNKNOWN"};
        } else {
          if ($privpass =~ /^0x/) {
            $priv = "-privkey => $privpass";
          } else {
            $priv = "-privpassword => $privpass";
          }
        }
      }
    } else {
      usage();
      exit $ERRORS{'UNKNOWN'}; ;
    }
  } # end snmpv3

  # start snmpv1 / snmpv2
  if ($snmp_version =~ /[12]/) {
    ($session, $error) = Net::SNMP->session(
      -hostname   => $hostname,
      -community  => $community,
      -port       => $port,
      -version    => $snmp_version,
      -maxmsgsize => $maxmsgsize
    );

    if ( ! defined($session)) {
      $state='UNKNOWN';
      $answer=$error;
      print ("$state: $answer");
      exit $ERRORS{$state};
    }

  } elsif ($snmp_version =~ /3/) {

    if ($seclevel eq 'noAuthNoPriv') {
      ($session, $error) = Net::SNMP->session(
        -hostname => $hostname,
        -port     => $port,
        -version  => $snmp_version,
        -username => $secname
      );
    } elsif ($seclevel eq 'authNoPriv') {
      ($session, $error) = Net::SNMP->session(
        -hostname     => $hostname,
        -port         => $port,
        -version      => $snmp_version,
        -username     => $secname,
        $auth,
        -authprotocol => $authproto
      );
    } elsif ($seclevel eq 'authPriv') {
      ($session, $error) = Net::SNMP->session(
        -hostname     => $hostname,
        -port         => $port,
        -version      => $snmp_version,
        -username     => $secname,
        $auth,
        -authprotocol => $authproto,
        $priv
      );
    }

    if ( ! defined($session)) {
      $state='UNKNOWN';
      $answer=$error;
      print ("$state: $answer");
      exit $ERRORS{$state};
    }

  } else {
    $state='UNKNOWN';
    print ("$state: No support for SNMP v$snmp_version yet\n");
    exit $ERRORS{$state};
  }
}
## End validation
