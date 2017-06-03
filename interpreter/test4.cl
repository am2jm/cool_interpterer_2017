class Main inherits IO {
a : A <- new A;
b : B <- new B;
c : C <- new C;
d : D <- new D;
q : IO;
e : A <- a;
z: Int;
main(): Object {
{
    z <- case b of
    n : Object => 5;
    n : D => 8;
    n : A => 10;
      esac;
      out_int(z);
}
};
};

class A inherits B{

};
class B inherits C{

};
class C inherits D{

};
class D{

};
