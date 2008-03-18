module guichan.util;

void remove(T) (ref T[] array, T item)
{
    foreach(int i, T t; array)
    {
      if( t is item )
      {
        array[i] = array[$-1];
        array.length = array.length - 1;
        return;
      }
    }
}

void remove_all(T) (ref T[] array, T item)
{
    foreach(int i, T t; array)
    {
      if( t is item )
      {
        array[i] = array[$-1];
        array.length = array.length - 1;
      }
    }
}

private class A {};

unittest
{
  
  A a = new A;
  A[] array = [a, new A,new A, a , a];

  assert( array[0] is a );
  assert( array.length == 5 );
  remove(array,a);
  assert( array.length == 4 );
  remove_all(array,a);
  assert( array.length == 2 );


}