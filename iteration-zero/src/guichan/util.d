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

void remove_index(T) (ref T[] array, int index)
{
    for(int i=index; i < array.length-1; ++i)
    {
        assert(i < array.length);
        array[i] = array[i+1];
    }
    array.length = array.length - 1;
}

void remove_index(T) (ref T[] array, uint index)
{
    for(int i=cast(int)index; i < array.length-1; ++i)
    {
        assert(i < array.length);
        array[i] = array[i+1];
    }
    array.length = array.length - 1;
}

void remove_range(T) (ref T[] array, int start, int end)
{
    for(int i= start; i < array.length-1; ++i)
    {
        assert(i + (end-start) < array.length);
        array[i] = array[i +  (end-start)];
    }
    array.length = array.length - (end-start);
}

void remove_all(T) (ref T[] array, T item)
{
    T t;
    for(int i = 0; i < array.length; ++i)
    {
      t = array[i];
      while( t is item && array.length > i)
      {
        array[i] = array[$-1];
        t = array[i];
        array.length = array.length - 1;
      }
    }
}

dstring[] splitlines(dstring text)
{
  dstring[] result;
  int last_i = 0;
  for(int i=0; i != text.length; ++i)
  {
    if( text[i] == '\n' || text[i] == '\r' )
    {
      result ~= text[last_i .. i];
      if( text.length == i + 1 )
        return result;
      if( (text[i] == '\n' &&  text[i+1] == '\r') || (text[i+1] == '\n' &&  text[i] == '\r') )
        ++i;
    }
  }
  return result;
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