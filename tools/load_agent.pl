#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;

use LWP::UserAgent;
use JSON::Any;

my $prog = $0;
$prog =~ s@.*/@@;

my $base = "http://127.0.0.1:8000";
my $freq = 3;
my $series = "new";
my $silent = 0;
my $help = 0;

GetOptions(
	"base=s" => \$base,
	"series=s" => \$series,
	"freq=s" => \$freq,
	"silent" => \$silent,
	"help" => \$help
) or usage(1);

usage(0) if $help;

if ( $series eq "new" ) {
	$series = create_series($base);
	print STDERR "$series series added\n" unless $silent;
}

while (1) {
	my $tick = JSON::Any->new()->encode( { timestamp => time(), value => get_value() } );
	if ( add_ticks($base, $series, $tick) ) {
		print STDERR "$tick added to $series\n" unless $silent;
	} else {
		warn "failed to add $tick to $series\n";
	}
	sleep $freq;
}

sub get_value {
	my ( $value ) = `uptime` =~ /load averages?: ([0-9\.]+)/;
	return 0 + $value;
}

sub usage {
	my $status = shift;
	print <<EOF;
usage: $prog [options]

  The system load agent for Probix.

  --base            base uri of the Probix server (default: $base)
  --freq   seconds  how often to send system load ticks (default: $freq)
  --series id       series id to send ticks to
  --silent          don't print debug information
  --help            print this help

EOF

	exit($status);
}


sub add_ticks {
	my ($base, $id, $ticks) = @_;
	my $uri = URI->new( $base );
	$uri->path_segments( 'series', $id );
	my $r = HTTP::Request->new( 'POST', $uri, [], $ticks );

	return _http_req($r)->is_success;
}

sub create_series {
	my $base = shift;

	my $uri = URI->new_abs( 'series', $base );
	$uri->query_form( { label => "system load average" } );
	my $r = HTTP::Request->new( 'POST' => $uri );

	my $res = _http_req($r);

	if ( 301 != $res->code ) {
		die sprintf "failed to create series: %s", $res->status_line;
	}

	my ( $id ) = $res->header( "Location" ) =~ /series\/(.+)/;

	return $id;
}

sub _http_req {
	my $r = shift;

	my $ua = LWP::UserAgent->new();

	$ua->agent("simple_agent/0.1");
	$ua->env_proxy;

	return $ua->request($r);
}


