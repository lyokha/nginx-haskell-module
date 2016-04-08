#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: gen_files.pl
#
#        USAGE: ./gen_files.pl -n number -s mean [-d deviation] [-p pattern]
#
#  DESCRIPTION: Generate random files
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Alexey Radkov (), 
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 28.02.2013 16:42:30
#     REVISION: ---
#===============================================================================

use strict;
use warnings;
use utf8;

use Getopt::Std;

my $VERSION = "1.0";
( my $PROGNAME = $0 ) =~ s/.*\///;

my $path = q'./test$$.data';

our ( $opt_n, $opt_s, $opt_d, $opt_p, $opt_h );


sub VERSION_MESSAGE
{
  PrintUsage();
  exit 0;
}

sub PrintUsage
{
  print "$PROGNAME, version $VERSION\n";
  print "Usage: $PROGNAME -n number -s mean [-d deviation] [-p pattern]\n";
  print "       $PROGNAME -h | --help\n";
  print "  -n number of files to generate\n";
  print "  -s mean size of a generated file (may use k, M and G units),\n";
  print "     sizes are distributed normally\n";
  print "  -d standard deviation of sizes distribution (may also use units),\n";
  print "     defaut value is (mean/5)\n";
  print "  -p pattern for paths (must include \$\$ for number substitution),\n";
  print "     default value is $path\n";
  print "  -h or --help print this help and exit\n";
}


# gaussian_rand() from Perl Cookbook
sub gaussian_rand
{
    my ($u1, $u2);  # uniformly distributed random numbers
    my $w;          # variance, then a weight
    my ($g1, $g2);  # gaussian-distributed numbers

    do {
        $u1 = 2 * rand() - 1;
        $u2 = 2 * rand() - 1;
        $w = $u1*$u1 + $u2*$u2;
    } while ( $w >= 1 );

    $w = sqrt( (-2 * log($w))  / $w );
    $g2 = $u1 * $w;
    $g1 = $u2 * $w;
    # return both if wanted, else just one
    return wantarray ? ($g1, $g2) : $g1;
}

sub random_bytes
{
    my $number = shift;

    return '' unless $number > 0;

    my $buf = "A" x $number;

    for my $i ( 0 .. $number - 1 )
    {
        my $rand = int( rand() * 256 );
        substr( $buf, $i, 1, chr $rand );
    }

    return $buf;
}

sub parse_size
{
    my $value = shift;

    return 0 if $value !~ /^(\d+)([kMG]*)$/;

    $value = $1;
    my $unit = $2;

    SWITCH_UNIT:
    {
        if ( $unit eq '' )
        {
            $unit = 1;
            last SWITCH_UNIT;
        }
        if ( $unit eq 'k' )
        {
            $unit = 1024;
            last SWITCH_UNIT;
        }
        if ( $unit eq 'M' )
        {
            $unit = 1024 * 1024;
            last SWITCH_UNIT;
        }
        if ( $unit eq 'G' )
        {
            $unit = 1024 * 1024 * 1024;
            last SWITCH_UNIT;
        }
    }

    return $value * $unit;
}


getopts( "n:s:d:p:h" );

if ( $opt_h )
{
  PrintUsage;
  exit 0;
}

die "Number of files is not defined" unless defined $opt_n;

my $number = int $opt_n;
die "Wrong number of files" unless defined $number && $number > 0;

die "Mean size of files is not defined" unless defined $opt_s;

my $mean = parse_size $opt_s;
die "Wrong mean size of files" unless defined $mean && $mean > 0;

my $disp = $mean / 5;
$disp = parse_size $opt_d if $opt_d;
die "Wrong standard deviation of files sizes" unless $disp > 0;

if ( $opt_p )
{
    die "No \$\$ in pattern" unless $opt_p =~ /\$\$/;
    $path = $opt_p;
}


for my $i ( 1 .. $number )
{
    my $len = 0;

    $len = gaussian_rand() * $disp + $mean while $len <= 0;

    ( my $file = $path ) =~ s/\$\$/$i/;

    #print "PATH: '$file', SIZE: $len\n";

    open( RESULT, "> $file" ) or die( $! );

    print RESULT random_bytes( $len );

    close RESULT;
}

