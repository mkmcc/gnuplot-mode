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

plot[-pi:pi][-1:1] sin(x),\
                   cos(x)

plot [-pi:pi] [-1.3:1.3] [-1:1] sin(t),\
                                t**2,\
                                cos(t) * sin(t)

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


# test of the "do for" loop which was introduced in gnuplot 4.6.
#   not working.
set term postscript eps enhanced color
set out "looptest.eps"
do for [i=0:4] {
   set title "On ".sprintf("page %d/10",i+1) # pointless convoluted way
   do for [mycolor in "blue red"]{           # a pointless second do loop
      plot sin(.5*pi*i*x) lc rgb mycolor title sprintf("sin(pi*x*%d/2)",i)
   }
}
