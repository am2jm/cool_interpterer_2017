
class A inherits IO{
f: String <- "hello";
x: Int <-54;
doStuff (x: Int, y: Int): Int {
    {
     x*y;
    }
  };
};



class Main inherits IO{


x: Int <- 3;
y: Int <- 2;

f: A <- new A;



main(): Object {
    {
      out_int(f.doStuff(x,y));
      out_string("\n");
    }
  };

};
