package scheme;

// (C) 2013 David A. Wheeler, released under MIT license.
// Provide a few traditional Lisp-like functions in Java,
// just barely enough functionality for sweet.g, but
// accurate for what it does.

public class Pair extends Object {

  private Object car_value, cdr_value;

  public Pair(Object x, Object y) { 
    this.car_value = x; this.cdr_value = y; 
  }


  public static Pair cons(Object x, Object y) {
    return new Pair(x, y);
  }

  public static Object car(Pair x) {
    return x.car_value;
  }

  public static Object cdr(Pair x) {
    return x.cdr_value;
  }

}

