binwidth = 0.25
set boxwidth binwidth
sum = 0
sum2 = 0

set key right bottom

s(x)          = ((sum=sum+1), 0)
s2(x)          = ((sum2=sum2+1), 0)
bin(x, width) = width*floor(x/width) + binwidth/2.0

plot "tmp_prior.txt" u ($1):(s($1))
plot "tmp_post.txt" u ($1):(s2($1))
plot "tmp_prior.txt" u (bin($1, binwidth)):(1.0/(binwidth*sum)) smooth freq w line, \
     "tmp_post.txt" u (bin($1, binwidth)):(1.0/(binwidth*sum2)) smooth freq w line
pause -1
