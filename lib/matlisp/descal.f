      subroutine descal (n,dx,incx,dy,incy)
      double precision dx(*),dy(*)
      integer incx,incy,n
*     Multiplies the vector X and Y element-wise.
*
      integer i,ix,iy
*     ..
      if (n.le.0) return
      if (incx.eq.1 .and. incy.eq.1) go to 20
*
*     code for unequal increments or equal increments not equal
*     to 1
*
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
         dy(iy) = dx(ix) * dy(iy)
         ix = ix + incx
         iy = iy + incy
   10 continue
      
      return
*
*       code for both increments equal to 1
*
   20 do 30 i = 1,n
         dy(i) = dy(i) * dx(i)

   30 continue
      
      return
      end
