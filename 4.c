
int fib( int n )
{
  int a = 0, b = 1, c, i;

	if (n == 0)
	  return 0;

  if (n == 1)
	  return 1;

  for (i = 0; i < n - 1; i++)
  {
    c = b;
    b = a + b;
    a = c;    
  }

  return b;
}

int main( void )
{
  int f = fib(10);

  return 0;
}