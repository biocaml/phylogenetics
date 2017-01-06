binwidth = 0.05
set boxwidth binwidth
sum = 0
sum2 = 0

s(x)          = ((sum=sum+1), 0)
s2(x)          = ((sum2=sum2+1), 0)
bin(x, width) = width*floor(x/width) + binwidth/2.0

plot "prior.txt" u ($1):(s($1))
plot "post.txt" u ($1):(s2($1))
plot "prior.txt" u (bin($1, binwidth)):(1.0/(binwidth*sum)) smooth freq w line, \
     "post.txt" u (bin($1, binwidth)):(1.0/(binwidth*sum2)) smooth freq w line
pause -1
