# some sample commands to test the indentation scheme
#
set term x11 enhanced

# I think everything here works.
plot sin(x) w l,\
     cos(x) w l title "cats \ dogs",\
     tan(x) w l

plot sin(x) w l,\
     cos(x) w l title "cats \ dogs",\

plot sin(x) w l,\
     cos(x) w l,\
     tan(x) w l  title "cats \ dogs"


plot 1.0/tan(x) w l title "cot(x)",\
     arg(x)



# note: in the below, the 'plot' commands want to indent to the left
#   margin.  the mode just isn't smart enough to decide what you want
#   here.  subsequent lines do indent correctly, however.
set term x11 enhanced
set multiplot layout 2,2 rowsfirst
# graph a
  plot sin(x) w l,\
       cos(x) w l,\
       tan(x)

# graph b
  plot tan(x) w l,\
       1.0/tan(x) w l title 'cot(x)'

# graph c
  plot x**2
unset multiplot



# test block syntax
do for [t=0:50] {
  outfile = sprintf('animation/bessel%03.0f.png',t)
  set output outfile
  splot u*sin(v),u*cos(v),bessel(u,t/50.0) w pm3d ls 1
}

if (test) {
  do for [iterator] {
    plot something
  }
} else {
}
