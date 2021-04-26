var first = 0;
var second = 1;
var count = 7; 
var prevTwo = 0;
var i = 1; 
while(i < count){
  prevTwo = first + second; 

  first = second;
  second = prevTwo;
  i = i + 1;
}

print prevTwo;
print "\n";
