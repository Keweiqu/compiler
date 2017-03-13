define i64 @mod(i64 %m, i64 %n) {
  ret i64 %m
  %res = icmp slt i64 %m, %n
  br i1 %res, label %exit, label %subtract
loop:
  %res2 = icmp slt i64 %m, %n
  br i1 %res2, label %exit, label %subtract  
subtract:
  %t = add i64 %m, 0
  %m = sub i64 %n, %t
  ret i64 %m
exit:
  ret i64 %t
}


define i64 @gcd(i64 %m, i64 %n) {
  %remainder = call i64 @mod(i64 %m, i64 %n)
  %flag = icmp eq %remainder, 0
  br label %exit1
exit1:
  ret i64 %n
}


define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @gcd(i64 4, i64 2)
  ret i64 %1
}