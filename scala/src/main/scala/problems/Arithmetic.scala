package problems

/*
 * 99 Problems, 31-41.
 * Arithmetic.
 */

object Arithmetic {

  // 31. Primality test
  def isPrime(n: Int): Boolean = ???

  // 32. Euclid's algorithm
  def gcd(n: Int, m: Int): Int =
    if (m == 0) { n } else { gcd(m, n%m) }

  // 33. Determine if n and m are coprime.
  // Two numbers are coprime if their greatest common divisor equals 1.
  def coprime(n: Int, m: Int): Boolean = gcd(n,m) == 1

  // 34. Euler's totient function phi(m)
  // Euler's so-called totient function phi(m) is defined as
  // the number of positive integers r (1 <= r < m) that are coprime to m.
  // Special case: phi(1) = 1
  def totientPhi(m: Int): Int = (1 to m).filter(coprime(_, m)).length

  // 35. Find all  prime factors of a given positive integer.
  def primeFactors(n: Int): List[Int] = ???

  // 36.  Find prime factors of a given positive integer and their multiplicity.
  def primeFactorsMult(n: Int): List[(Int,Int)] = ??

  // 37. Totient function, take 2.
  //
  // If the list of the prime factors of a number m is known in the form of
  // problem P36 then the function phi(m) can be efficiently calculated as
  //  follows:
  //
  // Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their
  // multiplicities) of a given number m. Then phi(m) can be calculated with the
  // following formula:
  //
  // phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1)
  //          + (p3 - 1) * p3 ** (m3 - 1) + ...
  //
  // Note that a ** b stands for the b'th power of a.
  def totientPhi2(m: Int): Int = ???

  // 38. compare totientPhi and totientPhi2

  // 39. List of all primes from lo to hi (inclusive).
  def primes(lo: Int, hi: Int): List[Int] = ???

  // 40. Goldbach's conjecture: every even positive number > 2
  // is the sum of two prime numbers.
  def goldbach(n: Int): (Int,Int) = ???

  // 41. List of all Goldbach compositions in range (inclusive)
  def goldbach(lo: Int, hi: Int): List[(Int,Int)] = ???
}
