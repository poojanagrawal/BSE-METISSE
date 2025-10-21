      SUBROUTINE zcnsts(z,zpars,path_to_tracks,path_to_he_tracks)
      IMPLICIT NONE
      INCLUDE 'const_bse.h'
      
      real*8 z,zpars(20)
      CHARACTER(LEN=*) path_to_tracks,path_to_he_tracks
      integer:: ierr
      
      if (sse_flag==0) then
          !WRITE(*,*) 'Calling SSE_zcnsts'
          CALL SSE_zcnsts(z,zpars)
      else
          !WRITE(*,*) 'Calling METISSE_zcnsts',using_METISSE
          CALL METISSE_zcnsts(z,zpars,path_to_tracks,
     &                       path_to_he_tracks,ierr)
          if (ierr/=0) STOP 'Fatal error: terminating METISSE'
      endif
      END
