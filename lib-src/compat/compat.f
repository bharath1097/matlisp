c     Compatibility routines for Matlisp.
c     
c     Fortran compilers differ on how a function returns a complex
c     result.  A compiler compatible with f2c will add a hidden first
c     parameter where the result of the function will be stored.  Other
c     compilers will use the standard C structure return convention.
c     
c     Currently there are two BLAS functions the return complex numbers:
c     ZDOTC and ZDOTU.

c     We don't want to deal with this difference in Matlisp, so we
c     define the following wrapper functions to call the real BLAS
c     routines using whatever the default convention is and then return
c     the result in the first argument to routine.
      subroutine mzdotc(result, n, x, incx, y, incy)
      complex*16 result
      complex*16 x(*), y(*)
      integer n, incx, incy
      external zdotc
      complex*16 zdotc
      result = zdotc(n, x, incx, y, incy)
      return
      end

      subroutine mzdotu(result, n, x, incx, y, incy)
      complex*16 result
      complex*16 x(*), y(*)
      integer n, incx, incy
      external zdotu
      complex*16 zdotu
      result = zdotu(n, x, incx, y, incy)
      return
      end
      
