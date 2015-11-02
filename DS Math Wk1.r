#Test 1:

#(1/x + 1/x^2)(3x^3+27) = ((x+1)/(x^2))*(3x^3+27)
#f(x) = ((x+1)/(x^2)) ; g(x) = (3x^3+27)
#(f(x)g(x))' = f'(x)g(x)+f(x)g'(x) "Product Rule"
#f'(x) = ((x^2)(1)-(x+1)(2x))/x^4 "Quotient Rule"
#f'(x)= (-x^2 - 2x)/x^4
#g'(x) = 9x^2
#((-x^2 - 2x)/x^4)(3x^3+27) + ((x+1)/(x^2))(9x^2)
#=(-3x^5-27x^2-6x^4-54x)/(x^4) + (9x^3+9x^2)/(x^2)
#=((-3x^5-6x^4-27x^2-54x) + (9x^5)+(9x^4))/(x^4)
#=6x^1+3x^0-27x^(-2)-54x^-3

D(expression((2*x^7 - x^2)*((x-1)/(x+1))),'x')
x <- 1
(2 * (7 * x^6) - 2 * x) * ((x - 1)/(x + 1)) + (2 * x^7 - x^2) * 
  (1/(x + 1) - (x - 1)/(x + 1)^2)

#(f(x)g(x))' = f'(x)g(x)+f(x)g'(x)
#f(x) = (x^2 + 1);g(x) = sec x
#f'(x) = 2x; g'(x) = sec x tan x
#2x(sec x) + (x^2 + 1)(sec x tan x)

#f(x) = (1/cot x) = (cot x)^-1 "Chain Rule"
#F'(x) = f'(g(x)) * g'(x) = 1/(cot^2 x)
#f'(x) = x^-2;g'(x) = -csc^2 x

D(expression(tan(x)),'x')
#1/cos(x)^2

#f(x) = tan x;f'(x) = sec^2 x; f''(x) = 2sec^2x tanx

D(expression(sin(x)^3),'x')
#3 * (cos(x) * sin(x)^2)

#f(x) = cos(x)^3; g(x) = x/(x+1)
#g'(x) = (1/(x+1)(x+1))
#f'(x) = -3sin(x)*cos(x)^2
D(expression(cos(x)^3),'x')
#(x/(x+1))(-3sin(x)*cos(x)^2) + (cos(x)^3)/((x+1)(x+1))

#g(x) = (x^4 - sec(4*x^2 - 2)) 
#= (x^4 - sec(4*x^2 - 1)*tan(4*x^2 - 1))*(8*x)
#f(x) = x^-4; f'(x) = -4x^-3
#g'(x) = (4x^3) - sec(f*x^2 - 1)*tan(4*x^2 - 1)*(8*x)
#f'(g(x)) * g'(x) 
#= -4(x^4 - sec(4*x^2 - 2))*((4x^3) - sec(f*x^2 - 1)*tan(4*x^2 - 1)*(8*x))

#sin(x) * cos(3*x+1)
D(expression(sin(x) * cos(3*x + 1)),'x')
#cos(x) * cos(3 * x + 1) - sin(x) * (sin(3 * x + 1) * 3)

D(expression(((1+x^2)/(1-x^2))^17),'x')
#17 * ((2 * x/(1 - x^2) + (1 + x^2) * (2 * x)/(1 - x^2)^2) * ((1 + 
#x^2)/(1 – x^2))^16)

#Test 2:
D(expression((x^2+1)^5),'x')
#10*x*(x^2 + 1)^4
D(expression(sqrt(1 - x)),'x')
#-(1/(2*sqrt(1 - x)))
#(1 - x)^0.5 * ((10*x*(x^2 + 1)^4)*((1 - x)^0.5) - ((x^2+1)^5)*(-(1/(2*sqrt(1 - x)))))/(1 - x)*(x^2 + 1)^5
D(expression( ( (x^2+1)^5 ) / ( sqrt(1 - x) ) ),'x')
# (
#  10 * x * (x^2 + 1)^4)*sqrt(1 - x) + ((x^2 + 1)^5) * (0.5 * 
#(1 - x)^-0.5)/(1 - x)
#   ) * (sqrt(1 - x) / (x^2 + 1)^5)
D(expression(log(((x^2 + 1)^5)/sqrt(1-x))),'x')

D(expression(log(log(x))),'x')
#1/x/log(x)

D(expression((x*log(x))/(1 + log(x))),'x')
#(log(x) + 1)^2 - (log(x)) / (1 + log(x))^2

D(expression((1 + log(t))/t),'t')
#1/t/t - (1 + log(t))/t^2

D(expression(t*sqrt(log(t))),'t')
#sqrt(log(t)) + t/2 * 1/t * 1/sqrt(log(t))

D(expression(log(10/x)),'x')
#-(10/x^2/(10/x))

D(expression(x^(1-x)),'x')
#x^((1 - x) - 1) * (1 - x) - x^(1 - x) * log(x)
#x^(1-x)*(1/x)*(1-x) - x^(1-x)*log(x)
#x^(1-x)*(1/x - 1 - log(x)

D(expression(e^(3*x - 1)),'e')
#e^((3 * x - 1) - 1) * (3 * x - 1)

D(expression((1-x)/exp(x)),'x')
#-(1/exp(x) + (1 - x) /exp(x))

D(expression(exp(sqrt(x)) + exp(-sqrt(x))),'x')
#exp(sqrt(x))/2sqrt(x)) - exp(-sqrt(x))/2sqrt(x)

D(expression(sin(2*exp(x))),'x')
#cos(2 * exp(x)) * (2 * exp(x))

D(expression(x*(3^(-5*x))),'x')
#(3^(-5 * x)) - x * (3^(-5 * x) * (log(3) * 5))


