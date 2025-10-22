      SUBROUTINE zcnsts(z,zpars)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 z,zpars(20)
      integer:: ierr
      
      if (sse_flag==0) then
          !WRITE(*,*) 'Calling SSE_zcnsts'
          CALL SSE_zcnsts(z,zpars)
      else
          !WRITE(*,*) 'Calling METISSE_zcnsts',using_METISSE
          CALL METISSE_zcnsts(z,zpars,ierr)
          if (ierr/=0) STOP 'Fatal error: terminating METISSE'
      endif
      END
