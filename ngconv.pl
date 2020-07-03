#!/usr/bin/perl

use strict;
use Term::ReadLine;
use Getopt::Long;

my $help_text = qq(NGconv
convert National Grid References to Easting and Northing coordinates.
written by Ben Soares for EDINA national datacentre
2003-10-30
usage:
NGconv [options] [inputfile]
command line options:
-v    show "." for each conversion
-vv   show "<NGref> => <easting>, <northing>" for each conversion
-point=(sw|nw|ne|se|mid|swne)
      convert to specified point(s) on box (default is "sw")
-o=<outputfile>
      defaults to <inputfile without extension>_en.csv
-includengr
      includes the national grid reference as the first item in the csv output
-h
-help outputs this text

);


my $opt = {};
$opt->{vv} = 1;
$opt->{i} = 0;
$opt->{point} = "sw";
$opt->{includengr} = 0;

GetOptions($opt,"includengr!","o=s","point=s","v!","vv!","h!","help!");
$opt->{vv} = 0 if $opt->{v};
$opt->{point} = lc($opt->{point});
my_die("--point must be one of ne, se, sw (default), ne, mid, swne.\n") unless grep($opt->{point} eq $_, qw(ne se sw ne mid swne));

if ($opt->{h} or $opt->{help}) {
  print ($help_text);
  exit 0;
}

$|=1;
my $digits = 5;

my $infile = $ARGV[0];

my $outfile = $opt->{o};
unless ($outfile) {
  $outfile = $infile;
  $outfile =~ s/\.\w{1,3}$//;
  $outfile .= "_en";
  $outfile .= ".csv";
}

while ($infile and not -f $infile) {
  my $term = Term::ReadLine->new("file input");
  print qq(File "$infile" does not exist!\n);
  $infile = $term->readline("Please enter another filename [<return> enters interactive mode] : ",$infile);
}

$opt->{i} = 1 unless ($infile and -f $infile);

if ($opt->{i}) {
  my $term = Term::ReadLine->new("interactive input");
  my $ng = "";
  while ($ng ne "q") {
    $ng =  $term->readline("Please enter National Grid Reference [q quits]: ",$ng);
    last if ($ng eq "q" or $ng eq "quit");
    $term->addhistory($ng);
    my @coords = convert($ng,$opt->{point});
    my $line = join(",",@coords)."\n";
    printf("%-14s => %s",$ng,$line);
  }
  exit 0;
}

open(IN,"<$infile") or my_die(qq(Failed to open input file "$infile".\n));
open(OUT,">$outfile") or my_die(qq(Failed to open output file "$outfile".\n));
while (<IN>) {
  my $ng = $_;
  chomp $ng;
  my @coords = convert($ng,$opt->{point});
  my $line = join(",",@coords)."\n";
  print OUT ($opt->{includengr}?"$ng,":"").$line;
  printf("%-14s => %s",$ng,$line) if ($opt->{vv});
  print "." if ($opt->{v});
}
close(IN);
close(OUT);
print "\n" if ($opt->{v});

exit 0;



sub my_warn {
  my $warning = shift;
  warn($warning);
}

sub my_die {
  my $warning = shift;
  die($warning);
}

sub convert {
  my ($ng,$point) = @_;
  $point = "sw" unless $point;
  unless ($ng =~ m/^([a-z])([a-z])\s*((\d{2}){0,$digits})\s*(([NS])([EW]))?$/i) {
    my_warn("National Grid reference not in correct form [$ng].\n");
    return undef;
  }
  my ($l1,$l2,$d,$q1,$q2) = (uc($1),uc($2),$3,uc($6),uc($7));
  my $d_length = length($d)/2;
  my $dx = substr($d,0,$d_length) * 10**($digits - $d_length);
  my $dy = substr($d,$d_length) * 10**($digits - $d_length);

  my $grid1 = ( (ord($l1) > ord("I"))?(ord($l1) - 1):ord($l1) ) - ord("A");
  my $grid2 = ( (ord($l2) > ord("I"))?(ord($l2) - 1):ord($l2) ) - ord("A");

  my $grid1_x = $grid1 % 5;
  my $grid1_y = 4 - int($grid1/5);

  my $grid2_x = $grid2 % 5;
  my $grid2_y = 4 - int($grid2/5);

  # put SV as origin
  $grid1_x -= 2;
  $grid1_y -= 1;

  my $precision = 10**($digits - $d_length);

  my $quadrant_x = ($q2 eq "E")?1:0;
  my $quadrant_y = ($q1 eq "N")?1:0;

  $precision /= 2 if ($q1 and $q2);

  my $easting = ($grid1_x * 500000) + ($grid2_x * 100000) + $dx + ($quadrant_x * $precision);
  my $northing = ($grid1_y * 500000) + ($grid2_y * 100000) + $dy + ($quadrant_y * $precision);

  my @ret = ();
  if ($point eq "sw") {
    @ret = ($easting, $northing);
  } elsif ($point eq "nw") {
    @ret = ($easting, $northing+$precision);
  } elsif ($point eq "se") {
    @ret =  ($easting+$precision, $northing);
  } elsif ($point eq "ne") {
    @ret =  ($easting+$precision, $northing+$precision);
  } elsif ($point eq "mid") {
    @ret =  ($easting+($precision/2), $northing+($precision/2));
  } elsif ($point eq "swne") {
    @ret =  ($easting, $northing, $easting+$precision, $northing+$precision);
  }

  return format_coordinates(@ret);
}

sub format_coordinates {
  my @formatted_coords = ();
  for my $coord (@_) {
    my $neg = $coord<0?1:0;
    my $dec = sprintf("%.2f",($coord-int($coord))*($neg?-1:1));
    $dec =~ s/^0*\././;
    $coord *= -1 if $neg;
    my $formatted_coord = ($neg?"-":"").sprintf("%06d",int($coord)).($dec!=0?$dec:"");
    push(@formatted_coords,$formatted_coord);
  }
  return @formatted_coords;
}
