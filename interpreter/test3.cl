


class D inherits IO{
x: Int <-54;

};

class C inherits IO{
f: D <- new D;
x: Int <-54;

};

class B inherits IO{
f: C <- new C;
x: Int <-54;

};


class A inherits IO{
f: B <- new B;
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
    if f=f then out_string("true") else out_string("false") fi
  };

};
