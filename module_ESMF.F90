!-----------------------------------------------------------------------
      MODULE ESMF
!
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: ESMF_Finalize,ESMF_END_ABORT
      integer, parameter :: ESMF_END_ABORT=10
!
      CONTAINS
!
      SUBROUTINE ESMF_Finalize(endflag)
      integer, intent(in), optional  :: endflag
      STOP ! call mpi_abort
      END SUBROUTINE ESMF_Finalize
!
      END MODULE ESMF
!-----------------------------------------------------------------------
