!-----------------------------------------------------------------------
!
      MODULE module_SOLVER_GRID_COMP
!
!-----------------------------------------------------------------------
!
!rv-mpi      USE MPI
      USE MODULE_KINDS
      USE MODULE_CONSTANTS        ,ONLY : A2,A3,A4,CAPPA,CP,ELIV,ELWV,EPSQ,G &
                                         ,P608,PQ0,R_D,TIW,DBZmin
      USE MODULE_DIAGNOSE         ,ONLY : MAX_FIELDS,MAX_FIELDS_HR,MAX_FIELDS_W6 &
                                         ,MAX_FIELDS_THO
      USE MODULE_RADIATION        ,ONLY : RADIATION
      USE MODULE_RA_GFDL          ,ONLY : GFDL_INIT,RDTEMP,TIME_MEASURE
      USE MODULE_RA_RRTM          ,ONLY : RRTM_INIT
      USE MODULE_TURBULENCE
      USE MODULE_SF_JSFC          ,ONLY : JSFC_INIT
      USE MODULE_BL_MYJPBL        ,ONLY : MYJPBL_INIT
      USE MODULE_LS_NOAHLSM       ,ONLY : DZSOIL,NOAH_LSM_INIT,NUM_SOIL_LAYERS
      USE MODULE_CU_BMJ           ,ONLY : BMJ_INIT
      USE MODULE_CU_SAS           ,ONLY : SAS_INIT
      USE MODULE_CU_SASHUR        ,ONLY : SASHUR_INIT
      USE MODULE_CU_SCALE         ,ONLY : SCALECU_INIT
      USE MODULE_CONVECTION
      USE MODULE_MICROPHYSICS_NMM ,ONLY : GSMDRIVE,MICRO_RESTART,UPDATE_WATER,TQADJUST
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: SOLVER_RUN
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVER_RUN &
(START_YEAR,START_MONTH,START_DAY,START_HOUR,START_MINUTE,START_SECOND,MINUTES_HISTORY &
,RESTART,gwdflg,CDMB,CLEFF,SIGFAC,factop,rlolev,dpmin,LONGWAVE,SHORTWAVE,CONVECTION &
,MICROPHYSICS,TURBULENCE,SFC_LAYER,LAND_SURFACE,CLDFRACTION,RHGRD,SPEC_ADV,UCMCALL &
,IVEGSRC,FRES,FR,FSL,FSS,ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP,NPHS,NRADS,NRADL,NPRECIP &
,sas_pgcon,sas_shal_pgcon,sas_shalconv,sas_mass_flux,sas_mommix,var_ric,coef_ric_l,coef_ric_s &
,ALPHA,SFENTH,D_SS,F_QC,F_QR,F_QI,F_QS,F_QG,F_NI,F_NR,MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE &
,SMC,STC,SH2O,MPRATES,PSGDT,Told,Tadj,F_ICE,F_RAIN,F_RIMEF,refl_10cm,Q2,OMGALF &
,O3,Z,CW,Q,T,U,V,RLWTT,RSWTT,EXCH_H,XLEN_MIX,CLDFRA,TRAIN,TCUCN,W_TOT,DUDT,DVDT &
,re_cloud,re_ice,re_snow,PINT,NCOUNT,NCFRST,NCFRCV,ISLTYP,IVGTYP,LPBL,ACFRCV,ACFRST &
,AKHS,AKHS_OUT,AKMS,AKMS_OUT,ALBASE,ALBEDO,ALBVB,ALBNB,ALBVD,ALBND &
,ALWIN,ALWOUT,ALWTOA,ASWIN,ASWOUT,ASWTOA &
,BGROFF,CFRACH,CFRACM,CFRACL,CNVBOT,CNVTOP,CMC,CPRATE,CUPPT,CZMEAN,CZEN,EPSR &
,FIS,HBOT,HBOTD,HBOTS,HTOP,HTOPD,HTOPS,GRNFLX,MAVAIL,MXSNAL,PBLH,MIXHT,PD,POTEVP &
,POTFLX,QSH,QWBS,QZ0,RADOT,RLWIN,RMOL,RSWIN,RSWINC,RSWOUT,RLWTOA,RSWTOA,SFCEVP,SFCEXC &
,SFCLHX,SFCSHX,SICE,SIGT4,SM,SMSTAV,SMSTOT,SNO,SNOWC,SNOPCX,SOILTB,SR,SSROFF,SST &
,SUBSHX,THS,THZ0,TSKIN,TWBS,USTAR,UZ0,VEGFRC,VZ0,Z0,Z0BASE,STDH,CROT,SROT,HSTDV &
,HCNVX,HASYW,HASYS,HASYSW,HASYNW,HLENW,HLENS,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX &
,ACSNOM,ACSNOW,ACPREC,ACPREC_TOT,acpcp_ra,acpcp_sn,acpcp_gr,CUPREC,PREC,CLDEFI,PSHLTR &
,PSFC,Q10,QSHLTR,T2,TH10,TSHLTR,U10,V10,TLMIN,TLMAX,ACUTIM,APHTIM &
,ARDLW,ARDSW,ASRFC,AVRAIN,AVCNVC,T02MAX,T02MIN,RH02MAX,RH02MIN,SPD10MAX,U10MAX,V10MAX &
,UPVVELMAX,DNVVELMAX,T10AVG,T10,PSFCAVG,AKHSAVG,AKMSAVG,SNOAVG,REFDMAX,UPHLMAX,GLAT,GLON &
,SI,TG,EPSL,EPSQ2,DSG2,SGML2,PSGML1,PDSG1,SG2,SGM,PSG1,NPREC,NCLOD,NHEAT,NRDLW &
,NRDSW,NSRFC,CU_PHYSICS,JULYR,NSTEPS_PER_CHECK,NSTEPS_PER_RESET,IDAT,DT,PT,NTIMESTEP)
!
!-----------------------------------------------------------------------
!***  INIT part (local variables)
!-----------------------------------------------------------------------
!
      USE MODULE_CONTROL,ONLY : CONSTS
!
!-----------------------------------------------------------------------
!***  END INIT part (move to physics initialize)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Variables from NAMELIST
!-----------------------------------------------------------------------
      INTEGER(kind=KINT) :: START_YEAR,START_MONTH,START_DAY,START_HOUR &
                           ,START_MINUTE,START_SECOND,MINUTES_HISTORY
!
      LOGICAL(kind=KLOG) :: RESTART
!
      LOGICAL(kind=KLOG) :: gwdflg
      REAL(KIND=KFPT)    :: CDMB,CLEFF,SIGFAC,factop,rlolev,dpmin
!
      CHARACTER(256)     :: LONGWAVE,SHORTWAVE,CONVECTION,MICROPHYSICS,TURBULENCE,SFC_LAYER,LAND_SURFACE
!
      CHARACTER(256)     :: CLDFRACTION
!
      REAL(kind=KFPT)    :: RHGRD
      LOGICAL(kind=KLOG) :: SPEC_ADV
!
      INTEGER(kind=KINT) :: UCMCALL
      INTEGER(kind=KINT) :: IVEGSRC
!
      REAL(kind=KFPT)    :: FRES,FR,FSL,FSS
      LOGICAL(kind=KLOG) :: ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP
!
      INTEGER(kind=KINT) :: NPHS,NRADS,NRADL,NPRECIP
!
      REAL(kind=KFPT)    :: sas_pgcon,sas_shal_pgcon,sas_shalconv,sas_mass_flux,sas_mommix &
                           ,var_ric,coef_ric_l,coef_ric_s,ALPHA,SFENTH
!-----------------------------------------------------------------------
!***  END Variables from NAMELIST
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Variables from internal state
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT),PARAMETER :: IME=1,IMS=1,JME=1,JMS=1,LM=35
      INTEGER(kind=KINT)           :: D_SS
      LOGICAL(kind=KLOG)           :: F_QC,F_QR,F_QI,F_QS,F_QG,F_NI,F_NR

      INTEGER(kind=KINT) :: has_reqc=0,has_reqi=0,has_reqs=0
      REAL(KIND=KFPT),DIMENSION(1:MICRO_RESTART) :: MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,NUM_SOIL_LAYERS) :: SMC,STC,SH2O
!
!-----------------------------------------------------------------------
!***  END Variables from internal state
!-----------------------------------------------------------------------
!
!
      LOGICAL(kind=KLOG) :: DISHEAT=.true.          ! true='consider diss heating'
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM-1) :: PSGDT

      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME):: PSP1
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM):: Told,Tadj,F_ICE,F_RAIN,F_RIMEF,refl_10cm,Q2,OMGALF &
                     ,O3,Z,CW,Q,T,U,V,RLWTT,RSWTT,EXCH_H,XLEN_MIX,CLDFRA,TRAIN &
                     ,TCUCN,W_TOT,DUDT,DVDT,QR,QS,QG,QI,QC,NI,NR &
                     ,TP1,QP1,DFI_TTEN &
                     ,re_cloud,re_ice,re_snow
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM+1):: PINT
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM,D_SS):: MPRATES
!---
      INTEGER(KIND=KINT),DIMENSION(IMS:IME,JMS:JME) :: NCOUNT,NCFRST,NCFRCV,ISLTYP,IVGTYP &
                                                      ,LPBL
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME) :: ACFRCV,ACFRST,AKHS,AKHS_OUT,AKMS &
                     ,AKMS_OUT,ALBASE,ALBEDO,ALBVB,ALBNB,ALBVD,ALBND &
                     ,ALWIN,ALWOUT,ALWTOA,ASWIN,ASWOUT,ASWTOA,BGROFF &
                     ,CFRACH,CFRACM,CFRACL,CNVBOT,CNVTOP,CMC,CPRATE,CUPPT,CZMEAN,CZEN &
                     ,EPSR,FIS,HBOT,HBOTD,HBOTS,HTOP,HTOPD,HTOPS,GRNFLX,MAVAIL,MXSNAL,PBLH &
                     ,MIXHT,PD,POTEVP,POTFLX,QSH,QWBS,QZ0,RADOT,RLWIN,RMOL,RSWIN,RSWINC &
                     ,RSWOUT,RLWTOA,RSWTOA,SFCEVP,SFCEXC,SFCLHX,SFCSHX,SICE,SIGT4,SM,SMSTAV &
                     ,SMSTOT,SNO,SNOWC,SNOPCX,SOILTB,SR,SSROFF,SST,SUBSHX,THS,THZ0,TSKIN &
                     ,TWBS,USTAR,UZ0,VEGFRC,VZ0,Z0,Z0BASE,STDH,CROT,SROT,HSTDV,HCNVX,HASYW &
                     ,HASYS,HASYSW,HASYNW,HLENW,HLENS,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX &
                     ,ACSNOM,ACSNOW,ACPREC,ACPREC_TOT,acpcp_ra,acpcp_sn,acpcp_gr,CUPREC,PREC &
                     ,CLDEFI,PSHLTR,PSFC,Q10,QSHLTR,T2,TH10,TSHLTR,U10,V10 &
                     ,TLMIN,TLMAX,ACUTIM,APHTIM,ARDLW,ARDSW,ASRFC,AVRAIN,AVCNVC &
                     ,T02MAX,T02MIN,RH02MAX,RH02MIN,SPD10MAX,U10MAX,V10MAX,UPVVELMAX,DNVVELMAX &
                     ,T10AVG,T10,PSFCAVG,AKHSAVG,AKMSAVG,SNOAVG,REFDMAX,UPHLMAX &
                     ,PRATEMAX,FPRATEMAX,GLAT,GLON,SI,TG,DDATA

      LOGICAL(kind=KLOG) :: FIRST_NMM,LISS_RESTART


      REAL(KIND=KFPT),DIMENSION(LM-1) :: EPSL
      REAL(KIND=KFPT),DIMENSION(LM)   :: EPSQ2
      REAL(KIND=KFPT),DIMENSION(LM)   :: DSG2,SGML2,PSGML1,PDSG1
      REAL(KIND=KFPT),DIMENSION(LM+1) :: SG2,SGM,PSG1
      INTEGER(kind=KINT) :: NPREC,NCLOD,NHEAT,NRDLW,NRDSW,NSRFC
      INTEGER(kind=KINT) :: CU_PHYSICS
      INTEGER(kind=KINT) :: MY_DOMAIN_ID=1,NUM_WATER=4
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER(kind=KINT) :: MYPE=0 !rv (MYPE for serial = 0)
!
      INTEGER(kind=KINT) :: I,J,K,L,NTIMESTEP,NTIMESTEP_RAD
!
      INTEGER(kind=KINT) :: JULDAY,JULYR,NSTEPS_PER_CHECK,NSTEPS_PER_RESET
!
      LOGICAL(kind=KLOG) :: USE_RADAR=.false.
!
!-----------------------------------------------------------------------
!***  SAVEs are for dereferenced constant variables.
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT),DIMENSION(8)  :: IDAT
!
      REAL(kind=KFPT) :: DT,RNPRECIP
!
      REAL(kind=KFPT) :: DYH,PT
!
      REAL(KIND=KFPT),DIMENSION(JMS:JME) :: DXH
!
      REAL(kind=KFPT),DIMENSION(:,:,:),ALLOCATABLE :: U_PHY,V_PHY
!
      REAL(kind=KFPT) :: JULIAN,XTIME
!
      INTEGER :: KK
!
      LOGICAL(kind=KLOG) :: CALL_LONGWAVE                               &
                           ,CALL_SHORTWAVE                              &
                           ,CALL_TURBULENCE                             &
                           ,CALL_PRECIP
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Allocate U/V for use in convection on mass points
!-----------------------------------------------------------------------
!
      IF(.NOT. ALLOCATED(U_PHY)) THEN
        ALLOCATE(U_PHY(IMS:IME,JMS:JME,1:LM),V_PHY(IMS:IME,JMS:JME,1:LM))
        U_PHY=0. ; V_PHY=0.
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----PHY_RUN START -----------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Call radiation so that updated fields are written to the
!***  history files after 0 hours.
!-----------------------------------------------------------------------
!
      IF(NTIMESTEP==0)THEN
         NTIMESTEP_RAD=NTIMESTEP
      ELSE
         NTIMESTEP_RAD=NTIMESTEP+1
      ENDIF
!
      CALL TIME_MEASURE(START_YEAR,START_MONTH,START_DAY,START_HOUR     &
                       ,START_MINUTE,START_SECOND                       &
                       ,NTIMESTEP_rad,DT                      &
                       ,JULDAY,JULYR,JULIAN,XTIME)
!
!    IDAT(1)=2013 ! yr
!    IDAT(2)=01   ! mnth
!    IDAT(3)=20   ! day
     IDAT(4)=0
!    IDAT(5)=23   ! hr
!    IDAT(6)=55   ! min
!    IDAT(7)=30   ! sec
     IDAT(8)=0
!
!-----------------------------------------------------------------------
!***  Dereference some internal state components for convenience.
!-----------------------------------------------------------------------
!
      RNPRECIP=1./REAL(NPRECIP)
!
!-----------------------------------------------------------------------
!***  MAIN PHYSICS LOOP OVER DOMAIN
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
!
!-----------------------------------------------------------------------
!***  Interpolate velocity components to mass points.
!-----------------------------------------------------------------------
        do k=1,lm
          u_phy(i,j,k)=u(i,j,k)
          v_phy(i,j,k)=v(i,j,k)
        enddo
!
!-----------------------------------------------------------------------
!***  At the appropriate times, reset the various min/max/average
!***  diagnostic fields to begin accumulating for the next period
!-----------------------------------------------------------------------
!
        IF(NTIMESTEP == 0 .or. MOD(NTIMESTEP,NSTEPS_PER_RESET)==0) THEN
          TLMAX(I,J)=-999.
          TLMIN(I,J)=999.
          T02MAX(I,J)=-999.
          T02MIN(I,J)=999.
          RH02MAX(I,J)=-999.
          RH02MIN(I,J)=999.
          SPD10MAX(I,J)=-999.
          U10MAX(I,J)=-999.
          V10MAX(I,J)=-999.
          UPVVELMAX(I,J)=-999.
          DNVVELMAX(I,J)=999.
          T10AVG(I,J)=0.
          T10(I,J)=0.
          PSFCAVG(I,J)=0.
          AKHSAVG(I,J)=0.
          AKMSAVG(I,J)=0.
          SNOAVG(I,J)=0.
          REFDMAX(I,J)=DBZmin
          PRATEMAX(I,J)=0
          FPRATEMAX(I,J)=0
          UPHLMAX(I,J)=-999.
          NCOUNT(I,J)=0
        ENDIF
!
        IF (mod(NTIMESTEP,NSTEPS_PER_CHECK) == 0 ) THEN
!
max_hrly: IF (TRIM(MICROPHYSICS) == 'fer') THEN
!
            CALL MAX_FIELDS(T(I:I,J:J,:),Q(I:I,J:J,:),U(I:I,J:J,:)           &
                           ,V(I:I,J:J,:),CW(I:I,J:J,:)                       &
                           ,F_RAIN(I:I,J:J,:),F_ICE(I:I,J:J,:)               &
                           ,F_RIMEF(I:I,J:J,:),Z(I:I,J:J,:)                  &
                           ,W_TOT(I:I,J:J,:),PINT(I:I,J:J,:)                 &
                           ,PD(I:I,J:J),PREC(I:I,J:J)                        &
                           ,CPRATE(I:I,J:J),HTOP(I:I,J:J)                    &
                           ,T2(I:I,J:J),U10(I:I,J:J),V10(I:I,J:J)            &
                           ,PSHLTR(I:I,J:J),TSHLTR(I:I,J:J)                  &
                           ,QSHLTR(I:I,J:J)                                  &
                           ,SGML2,PSGML1                                     &
                           ,REFDMAX(I:I,J:J),PRATEMAX(I:I,J:J)               &
                           ,FPRATEMAX(I:I,J:J),SR(I:I,J:J)                   &
                           ,UPVVELMAX(I:I,J:J),DNVVELMAX(I:I,J:J)            &
                           ,TLMAX(I:I,J:J),TLMIN(I:I,J:J)                    &
                           ,T02MAX(I:I,J:J),T02MIN(I:I,J:J)                  &
                           ,RH02MAX(I:I,J:J),RH02MIN(I:I,J:J)                &
                           ,U10MAX(I:I,J:J),V10MAX(I:I,J:J)                  &
                           ,TH10(I:I,J:J),T10(I:I,J:J)                       &
                           ,SPD10MAX(I:I,J:J),T10AVG(I:I,J:J)                &
                           ,PSFCAVG(I:I,J:J)                                 &
                           ,AKHS(I:I,J:J),AKMS(I:I,J:J)                      &
                           ,AKHSAVG(I:I,J:J),AKMSAVG(I:I,J:J)                &
                           ,SNO(I:I,J:J),SNOAVG(I:I,J:J)                     &
                           ,UPHLMAX(I:I,J:J)                                 &
                           ,DT,NPHS,NTIMESTEP                                &
                           ,FIS(I:I,J:J)                                     &
                           ,I,I,J,J                                          &
                           ,I,I,J,J                                          &
                           ,I,J                                              &
                           ,I,I,J,J                                          &
                           ,LM,NCOUNT(I,J),FIRST_NMM                         &
                           ,MY_DOMAIN_ID)
!
          ELSEIF (TRIM(MICROPHYSICS) == 'fer_hires') THEN  max_hrly
!
            CALL MAX_FIELDS_HR(T(I:I,J:J,:),Q(I:I,J:J,:),U(I:I,J:J,:)            &
                              ,V(I:I,J:J,:),CW(I:I,J:J,:)                        &
                              ,F_RAIN(I:I,J:J,:),F_ICE(I:I,J:J,:)                &
                              ,F_RIMEF(I:I,J:J,:),Z(I:I,J:J,:)                   &
                              ,W_TOT(I:I,J:J,:),refl_10cm(I:I,J:J,:)             &
                              ,PINT(I:I,J:J,:),PD(I:I,J:J),PREC(I:I,J:J)         &
                              ,CPRATE(I:I,J:J),HTOP(I:I,J:J)                     &
                              ,T2(I:I,J:J),U10(I:I,J:J),V10(I:I,J:J)             &
                              ,PSHLTR(I:I,J:J),TSHLTR(I:I,J:J)                   &
                              ,QSHLTR(I:I,J:J)                                   &
                              ,SGML2,PSGML1                                      &
                              ,REFDMAX(I:I,J:J),PRATEMAX(I:I,J:J)                &
                              ,FPRATEMAX(I:I,J:J),SR(I:I,J:J)                    &
                              ,UPVVELMAX(I:I,J:J),DNVVELMAX(I:I,J:J)             &
                              ,TLMAX(I:I,J:J),TLMIN(I:I,J:J)                     &
                              ,T02MAX(I:I,J:J),T02MIN(I:I,J:J)                   &
                              ,RH02MAX(I:I,J:J),RH02MIN(I:I,J:J)                 &
                              ,U10MAX(I:I,J:J),V10MAX(I:I,J:J)                   &
                              ,TH10(I:I,J:J),T10(I:I,J:J)                        &
                              ,SPD10MAX(I:I,J:J),T10AVG(I:I,J:J)                 &
                              ,PSFCAVG(I:I,J:J)                                  &
                              ,AKHS(I:I,J:J),AKMS(I:I,J:J)                       &
                              ,AKHSAVG(I:I,J:J),AKMSAVG(I:I,J:J)                 &
                              ,SNO(I:I,J:J),SNOAVG(I:I,J:J)                      &
                              ,UPHLMAX(I:I,J:J)                                  &
                              ,DT,NPHS,NTIMESTEP                                 &
                              ,FIS(I:I,J:J)                                      &
                              ,I,I,J,J                                           &
                              ,I,I,J,J                                           &
                              ,I,J                                               &
                              ,I,I,J,J                                           &
                              ,LM,NCOUNT(I,J),FIRST_NMM                          &
                              ,MY_DOMAIN_ID)
!
          ELSEIF (TRIM(MICROPHYSICS) == 'wsm6') THEN  max_hrly
!
            CALL MAX_FIELDS_W6(T(I:I,J:J,:),Q(I:I,J:J,:),U(I:I,J:J,:)     &
                              ,V(I:I,J:J,:),Z(I:I,J:J,:),W_TOT(I:I,J:J,:) &
                              ,QR(I:I,J:J,:),QS(I:I,J:J,:),QG(I:I,J:J,:)  &
                              ,PINT(I:I,J:J,:),PD(I:I,J:J),PREC(I:I,J:J)  &
                              ,CPRATE(I:I,J:J),HTOP(I:I,J:J)              &
                              ,T2(I:I,J:J),U10(I:I,J:J),V10(I:I,J:J)      &
                              ,PSHLTR(I:I,J:J),TSHLTR(I:I,J:J)            &
                              ,QSHLTR(I:I,J:J)                            &
                              ,SGML2,PSGML1                               &
                              ,REFDMAX(I:I,J:J),PRATEMAX(I:I,J:J)         &
                              ,FPRATEMAX(I:I,J:J),SR(I:I,J:J)             &
                              ,UPVVELMAX(I:I,J:J),DNVVELMAX(I:I,J:J)      &
                              ,TLMAX(I:I,J:J),TLMIN(I:I,J:J)              &
                              ,T02MAX(I:I,J:J),T02MIN(I:I,J:J)            &
                              ,RH02MAX(I:I,J:J),RH02MIN(I:I,J:J)          &
                              ,U10MAX(I:I,J:J),V10MAX(I:I,J:J)            &
                              ,TH10(I:I,J:J),T10(I:I,J:J)                 &
                              ,SPD10MAX(I:I,J:J),T10AVG(I:I,J:J)          &
                              ,PSFCAVG(I:I,J:J)                           &
                              ,AKHS(I:I,J:J),AKMS(I:I,J:J)                &
                              ,AKHSAVG(I:I,J:J),AKMSAVG(I:I,J:J)          &
                              ,SNO(I:I,J:J),SNOAVG(I:I,J:J)               &
                              ,UPHLMAX(I:I,J:J)                           &
                              ,DT,NPHS,NTIMESTEP                          &
                              ,FIS(I:I,J:J)                               &
                              ,I,I,J,J                                    &
                              ,I,I,J,J                                    &
                              ,I,J                                        &
                              ,I,I,J,J                                    &
                              ,LM                                         &
                              ,NCOUNT(I,J),FIRST_NMM                      &
                              ,MY_DOMAIN_ID)
!
          ELSEIF (TRIM(MICROPHYSICS) == 'thompson') THEN  max_hrly
!
            CALL MAX_FIELDS_THO(T(I:I,J:J,:),Q(I:I,J:J,:),U(I:I,J:J,:)    &
                              ,V(I:I,J:J,:),Z(I:I,J:J,:),W_TOT(I:I,J:J,:) &
                              ,refl_10cm(I:I,J:J,:)                       &
                              ,PINT(I:I,J:J,:),PD(I:I,J:J),PREC(I:I,J:J)  &
                              ,CPRATE(I:I,J:J),HTOP(I:I,J:J)              &
                              ,T2(I:I,J:J),U10(I:I,J:J),V10(I:I,J:J)      &
                              ,PSHLTR(I:I,J:J),TSHLTR(I:I,J:J)            &
                              ,QSHLTR(I:I,J:J)                            &
                              ,SGML2,PSGML1                               &
                              ,REFDMAX(I:I,J:J),PRATEMAX(I:I,J:J)         &
                              ,FPRATEMAX(I:I,J:J),SR(I:I,J:J)             &
                              ,UPVVELMAX(I:I,J:J),DNVVELMAX(I:I,J:J)      &
                              ,TLMAX(I:I,J:J),TLMIN(I:I,J:J)              &
                              ,T02MAX(I:I,J:J),T02MIN(I:I,J:J)            &
                              ,RH02MAX(I:I,J:J),RH02MIN(I:I,J:J)          &
                              ,U10MAX(I:I,J:J),V10MAX(I:I,J:J)            &
                              ,TH10(I:I,J:J),T10(I:I,J:J)                 &
                              ,SPD10MAX(I:I,J:J),T10AVG(I:I,J:J)          &
                              ,PSFCAVG(I:I,J:J)                           &
                              ,AKHS(I:I,J:J),AKMS(I:I,J:J)                &
                              ,AKHSAVG(I:I,J:J),AKMSAVG(I:I,J:J)          &
                              ,SNO(I:I,J:J),SNOAVG(I:I,J:J)               &
                              ,UPHLMAX(I:I,J:J)                           &
                              ,DT,NPHS,NTIMESTEP                          &
                              ,FIS(I:I,J:J)                               &
                              ,I,I,J,J                                    &
                              ,I,I,J,J                                    &
                              ,I,J                                        &
                              ,I,I,J,J                                    &
                              ,LM                                         &
                              ,NCOUNT(I,J),FIRST_NMM                      &
                              ,MY_DOMAIN_ID)
!
          ENDIF  max_hrly
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Set logical switches for calling each of the Physics schemes.
!-----------------------------------------------------------------------
!
        CALL_SHORTWAVE=MOD(NTIMESTEP_RAD,NRADS)==0
        CALL_LONGWAVE=MOD(NTIMESTEP_RAD,NRADL)==0
        CALL_TURBULENCE=MOD(NTIMESTEP,NPHS)==0
        CALL_PRECIP=MOD(NTIMESTEP,NPRECIP)==0
!
!-----------------------------------------------------------------------
!***  Update WATER array from CWM, F_ICE, F_RAIN for Ferrier
!***  microphysics but only if any of the Physics subroutines
!***  are called (subroutine UPDATE_WATER is after subroutine
!***  PHYSICS_INITIALIZE in this module).
!
!***  Expanded to also update CWM, F_ICE, F_RAIN, F_RIMEF for non-Ferrier
!***  microphysics.
!-----------------------------------------------------------------------
!
        update_wtr: IF((MICROPHYSICS=='fer'                   &
                                   .OR.                       &
                        MICROPHYSICS=='fer_hires'             &
                                   .OR.                       &
                        MICROPHYSICS=='gfs'                   &
                                   .OR.                       &
                        MICROPHYSICS=='wsm6'                  &
                                   .OR.                       &
                        MICROPHYSICS=='thompson')             &
                                   .AND.                      &
                       (CALL_SHORTWAVE .OR. CALL_LONGWAVE .OR.&
                        CALL_TURBULENCE .OR. CALL_PRECIP) ) THEN
!
          CALL UPDATE_WATER(CW(I:I,J:J,:)                     &
                           ,F_ICE(I:I,J:J,:)                  &
                           ,F_RAIN(I:I,J:J,:)                 &
                           ,F_RIMEF(I:I,J:J,:)                &
                           ,T(I:I,J:J,:)                      &
                           ,QC(I:I,J:J,:)                     &
                           ,QR(I:I,J:J,:)                     &
                           ,QS(I:I,J:J,:)                     &
                           ,QI(I:I,J:J,:)                     &
                           ,QG(I:I,J:J,:)                     &
                           ,MICROPHYSICS                      &
                           ,SPEC_ADV                          &
                           ,NTIMESTEP                         &
                           ,LM,I,I,J,J)
!
        ENDIF update_wtr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Call the individual physical processes.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Radiation
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
        radiatn: IF(CALL_SHORTWAVE.OR.CALL_LONGWAVE)THEN
!-----------------------------------------------------------------------
!
          CALL RADIATION(NTIMESTEP_RAD                      &
                        ,DT,JULDAY,JULYR,XTIME,JULIAN       &
                        ,START_HOUR,NPHS                    &
                        ,GLAT(I:I,J:J),GLON(I:I,J:J)        &
                        ,NRADS,NRADL                        &
                        ,DSG2,SGML2,SG2                     &
                        ,PDSG1,PSGML1                       &
                        ,PSG1                               &
                        ,PT,PD(I:I,J:J)                     &
                        ,T(I:I,J:J,:),Q(I:I,J:J,:)          &
                        ,THS(I:I,J:J),ALBEDO(I:I,J:J)       &
                        ,QC(I:I,J:J,:),QR(I:I,J:J,:)        &
                        ,QI(I:I,J:J,:),QS(I:I,J:J,:)        &
                        ,QG(I:I,J:J,:),NI(I:I,J:J,:)        &
                        ,F_QC,F_QR                          &
                        ,F_QI,F_QS,F_QG                     &
                        ,F_NI                               &
                        ,NUM_WATER                          &
                        ,SM(I:I,J:J),CLDFRA(I:I,J:J,:)      &
                        ,RLWTT(I:I,J:J,:),RSWTT(I:I,J:J,:)  &
                        ,RLWIN(I:I,J:J),RSWIN(I:I,J:J)      &
                        ,RSWINC(I:I,J:J),RSWOUT(I:I,J:J)    &
                        ,RLWTOA(I:I,J:J),RSWTOA(I:I,J:J)    &
                        ,CZMEAN(I:I,J:J),SIGT4(I:I,J:J)     &
                        ,CFRACL(I:I,J:J),CFRACM(I:I,J:J)    &
                        ,CFRACH(I:I,J:J)                    &
                        ,ACFRST(I:I,J:J),NCFRST(I:I,J:J)    &
                        ,ACFRCV(I:I,J:J),NCFRCV(I:I,J:J)    &
                        ,CUPPT(I:I,J:J),SNO(I:I,J:J)        &
                        ,HTOP(I:I,J:J),HBOT(I:I,J:J)        &
                        ,SHORTWAVE,LONGWAVE                 &
                        ,CLDFRACTION                        &
                        ,DYH                                &
!---- RRTM part ---------------------------------------------------------
                        ,IDAT                               &
                        ,CW(I:I,J:J,:),O3(I:I,J:J,:)        &
                        ,F_ICE(I:I,J:J,:),F_RAIN(I:I,J:J,:) &
                        ,F_RIMEF(I:I,J:J,:)                 &
                        ,SI(I:I,J:J),TSKIN(I:I,J:J)         &
                        ,Z0(I:I,J:J),SICE(I:I,J:J)          &
                        ,MXSNAL(I:I,J:J),SGM                &
                        ,STDH(I:I,J:J),OMGALF(I:I,J:J,:)    &
                        ,ALBVB(I:I,J:J),ALBNB(I:I,J:J)      &  ! vis+uv & near IR beam albedos
                        ,ALBVD(I:I,J:J),ALBND(I:I,J:J)      &  ! vis+uv & near IR diffuse albedos
                        ,SNOWC(I:I,J:J)                     &
!------------------------------------------------------------------------
                        ,LM,I,J,MYPE)
!
        ENDIF radiatn
!
!-----------------------------------------------------------------------
!***  Empty the ACFRST and ACFRCV accumulation arrays if it is time
!***  to do so prior to their being updated by the radiation.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,NCLOD)==0)THEN
          ACFRST(I,J)=0.
          ACFRCV(I,J)=0.
          NCFRST(I,J)=0
          NCFRCV(I,J)=0
        ENDIF
!
!-----------------------------------------------------------------------
!***  Update the temperature with the radiative tendency.
!-----------------------------------------------------------------------
!
        CALL RDTEMP(NTIMESTEP,DT,JULDAY,JULYR,START_HOUR      &
                   ,GLAT(I:I,J:J),GLON(I:I,J:J)               &
                   ,CZEN(I:I,J:J),CZMEAN(I:I,J:J)             &
                   ,T(I:I,J:J,:)                              &
                   ,RSWTT(I:I,J:J,:)                          &
                   ,RLWTT(I:I,J:J,:)                          &
                   ,I,I,J,J,LM                                &
                   ,I,I,J,J                                   &
                   ,I,I,J,J                                   &
                   ,I,I,J,J)
!
!-----------------------------------------------------------------------
!***  Empty the accumulators of sfc energy flux and sfc hydrology if
!***  it is time to do so prior to their being updated by turbulence.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,NRDLW)==0)THEN
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ARDLW(I,J) =0.                                       !<-- An artificial 2-D array
        ENDIF
!
        IF(MOD(NTIMESTEP,NRDSW)==0)THEN
          ASWIN(I,J)=0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          ARDSW(I,J) =0.                                       !<-- An artificial 2-D array
        ENDIF
!
        IF(MOD(NTIMESTEP,NSRFC)==0)THEN
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          POTFLX(I,J)=0.
          ASRFC(I,J) =0.                                       !<-- An artificial 2-D array
        ENDIF
!
        IF(MOD(NTIMESTEP,NPREC)==0)THEN
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
        ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Turbulence, Sfc Layer, and Land Surface
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        IF(CALL_TURBULENCE)THEN
!
          CALL TURBL(NTIMESTEP,DT,NPHS,NUM_SOIL_LAYERS,DZSOIL        &
                    ,u_phy(I:I,J:J,:),v_phy(I:I,J:J,:)               &
                    ,DSG2,SGML2,SG2,PDSG1                            &
                    ,PSGML1,PSG1,PT,EPSL,EPSQ2                       &
                    ,SM(I:I,J:J),CZEN(I:I,J:J),CZMEAN(I:I,J:J)       &
                    ,SIGT4(I:I,J:J),RLWIN(I:I,J:J),RSWIN(I:I,J:J)    &
                    ,RADOT(I:I,J:J) ,RLWTT(I:I,J:J,:)                &
                    ,RSWTT(I:I,J:J,:),PD(I:I,J:J),T(I:I,J:J,:)       &
                    ,Q(I:I,J:J,:),CW(I:I,J:J,:),F_ICE(I:I,J:J,:)     &
                    ,F_RAIN(I:I,J:J,:),F_RIMEF(I:I,J:J,:)            &
                    ,SR(I:I,J:J),Q2(I:I,J:J,:),DUDT(I:I,J:J,:)       &
                    ,DVDT(I:I,J:J,:),THS(I:I,J:J),TSKIN(I:I,J:J)     &
                    ,SST(I:I,J:J),PREC(I:I,J:J),SNO(I:I,J:J)         &
                    ,SNOWC(I:I,J:J),QC(I:I,J:J,:),QR(I:I,J:J,:)      &
                    ,QI(I:I,J:J,:),QS(I:I,J:J,:),QG(I:I,J:J,:)       &
                    ,F_QC,F_QR,F_QI,F_QS,F_QG                        &
                    ,FIS(I:I,J:J),Z0(I:I,J:J),Z0BASE(I:I,J:J)        &
                    ,USTAR(I:I,J:J),PBLH(I:I,J:J),LPBL(I:I,J:J)      &
                    ,XLEN_MIX(I:I,J:J,:),RMOL(I:I,J:J)               &
                    ,EXCH_H(I:I,J:J,:),AKHS(I:I,J:J),AKMS(I:I,J:J)   &
                    ,AKHS_OUT(I:I,J:J),AKMS_OUT(I:I,J:J)             &
                    ,THZ0(I:I,J:J),QZ0(I:I,J:J)                      &
                    ,UZ0(I:I,J:J),VZ0(I:I,J:J)                       &
                    ,QSH(I:I,J:J),MAVAIL(I:I,J:J)                    &
                    ,STC(I:I,J:J,:),SMC(I:I,J:J,:),CMC(I:I,J:J)      &
                    ,SMSTAV(I:I,J:J),SMSTOT(I:I,J:J)                 &
                    ,SSROFF(I:I,J:J),BGROFF(I:I,J:J)                 &
                    ,IVGTYP(I:I,J:J),ISLTYP(I:I,J:J)                 &
                    ,VEGFRC(I:I,J:J),GRNFLX(I:I,J:J)                 &
                    ,SFCEXC(I:I,J:J),ACSNOW(I:I,J:J)                 &
                    ,ACSNOM(I:I,J:J),SNOPCX(I:I,J:J)                 &
                    ,SICE(I:I,J:J),TG(I:I,J:J),SOILTB(I:I,J:J)       &
                    ,ALBASE(I:I,J:J),MXSNAL(I:I,J:J)                 &
                    ,ALBEDO(I:I,J:J),SH2O(I:I,J:J,:),SI(I:I,J:J)     &
                    ,EPSR(I:I,J:J),U10(I:I,J:J),V10(I:I,J:J)         &
                    ,TH10(I:I,J:J),Q10(I:I,J:J),TSHLTR(I:I,J:J)      &
                    ,QSHLTR(I:I,J:J),PSHLTR(I:I,J:J),PSFC(I:I,J:J)   &
                    ,T2(I:I,J:J),TWBS(I:I,J:J),QWBS(I:I,J:J)         &
                    ,SFCSHX(I:I,J:J),SFCLHX(I:I,J:J),SFCEVP(I:I,J:J) &
                    ,POTEVP(I:I,J:J),POTFLX(I:I,J:J),SUBSHX(I:I,J:J) &
                    ,APHTIM(I:I,J:J),ARDSW(I:I,J:J),ARDLW(I:I,J:J)   &
                    ,ASRFC(I:I,J:J),CROT(I:I,J:J),SROT(I:I,J:J)      &
                    ,MIXHT(I:I,J:J),HSTDV(I:I,J:J),HCNVX(I:I,J:J)    &
                    ,HASYW(I:I,J:J),HASYS(I:I,J:J),HASYSW(I:I,J:J)   &
                    ,HASYNW(I:I,J:J),HLENW(I:I,J:J),HLENS(I:I,J:J)   &
                    ,HLENSW(I:I,J:J),HLENNW(I:I,J:J),HANGL(I:I,J:J)  &
                    ,HANIS(I:I,J:J),HSLOP(I:I,J:J),HZMAX(I:I,J:J)    &
                    ,CDMB,CLEFF,SIGFAC                               &
                    ,FACTOP,RLOLEV,DPMIN                             &
                    ,RSWOUT(I:I,J:J),RSWTOA(I:I,J:J),RLWTOA(I:I,J:J) &
                    ,ASWIN(I:I,J:J),ASWOUT(I:I,J:J),ASWTOA(I:I,J:J)  &
                    ,ALWIN(I:I,J:J),ALWOUT(I:I,J:J),ALWTOA(I:I,J:J)  &
                    ,GWDFLG,.false.,DDATA(I:I,J:J),UCMCALL           &
                    ,IVEGSRC,TURBULENCE,SFC_LAYER                    &
                    ,LAND_SURFACE,MICROPHYSICS,LISS_RESTART          &
                    ,VAR_RIC,COEF_RIC_L,COEF_RIC_S                   &
                    ,DISHEAT,ALPHA,SFENTH                            &
                    ,I,I,J,J,LM                                      &
                    ,I,I,J,J                                         &
                    ,I,I,J,J)
!
!-----------------------------------------------------------------------
!
          DO L=1,LM
            u_phy(i,j,l)=u_phy(i,j,l)+DUDT(i,j,l)*DT*NPHS
            v_phy(i,j,l)=v_phy(i,j,l)+DVDT(i,j,l)*DT*NPHS
          ENDDO
!
!-----------------------------------------------------------------------
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Empty the accumulators of precipitation and latent heating if is
!***  is time prior to their being updated by convection/microphysics.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,NPREC)==0)THEN
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
        ENDIF
!
        IF(MOD(NTIMESTEP,NHEAT)==0)THEN
          AVCNVC(I,J)=0.
          AVRAIN(I,J)=0.
!
          DO L=1,LM
            TRAIN(I,J,L)=0.
            TCUCN(I,J,L)=0.
            do KK=1,d_ss
              MPRATES(I,J,L,KK)=0.
            enddo
          ENDDO
        ENDIF    !-- IF(MOD(NTIMESTEP,NHEAT)==0)THEN
!
!-----------------------------------------------------------------------
!***  1 of 3 calls to CLTEND, save Told array before convection & microphysics
!-----------------------------------------------------------------------
!
        cld_tend1: IF(CALL_PRECIP .AND. NPRECIP>1) THEN
          DO K=1,LM
            Told(I,J,K)=T(I,J,K)
          ENDDO
        ENDIF cld_tend1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Convection
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        IF(CALL_PRECIP.AND.CONVECTION/='none')THEN
!
!-----------------------------------------------------------------------
          IF(CONVECTION=='bmj' .OR. &
             CONVECTION=='sas' .OR. &
             CONVECTION=='scalecu' .OR. &
             CONVECTION=='sashur') THEN
!
            CALL CUCNVC(NTIMESTEP,DT,NPRECIP,NRADS,NRADL                       &
                       ,MINUTES_HISTORY,ENTRAIN,NEWALL                         &
                       ,NEWSWAP,NEWUPUP,NODEEP,FRES                            &
                       ,FR,FSL,FSS,DYH,RESTART                                 &
                       ,CLDEFI(I:I,J:J),u_phy(I:I,J:J,:),v_phy(I:I,J:J,:)      &
                       ,F_ICE(I:I,J:J,:),F_RAIN(I:I,J:J,:)                     &
                       ,QC(I:I,J:J,:),QR(I:I,J:J,:)                            &
                       ,QI(I:I,J:J,:),QS(I:I,J:J,:),QG(I:I,J:J,:)              &
                       ,F_QC,F_QR,F_QI,F_QS,F_QG                               &
                       ,DSG2,SGML2,SG2,PDSG1                                   &
                       ,PSGML1,PSG1,DXH(J:J),PT                                & !rv - take DXH out
                       ,PD(I:I,J:J),T(I:I,J:J,:),Q(I:I,J:J,:)                  &
                       ,CW(I:I,J:J,:),TCUCN(I:I,J:J,:)                         &
                       ,OMGALF(I:I,J:J,:),FIS(I:I,J:J)                         &
                       ,PREC(I:I,J:J),ACPREC(I:I,J:J),CUPREC(I:I,J:J)          &
                       ,ACPREC_TOT(I:I,J:J),CUPPT(I:I,J:J)                     &
                       ,CPRATE(I:I,J:J),CNVBOT(I:I,J:J)                        &
                       ,CNVTOP(I:I,J:J),SM(I:I,J:J),LPBL(I:I,J:J)              &
                       ,HTOP(I:I,J:J),HTOPD(I:I,J:J),HTOPS(I:I,J:J)            &
                       ,HBOT(I:I,J:J),HBOTD(I:I,J:J),HBOTS(I:I,J:J)            &
                       ,AVCNVC(I:I,J:J),ACUTIM(I:I,J:J)                        &
                       ,RSWIN(I:I,J:J),RSWOUT(I:I,J:J)                         &
                       ,CONVECTION,CU_PHYSICS,MICROPHYSICS                     &
                       ,SICE(I:I,J:J),QWBS(I:I,J:J),TWBS(I:I,J:J)              &
                       ,PBLH(I:I,J:J),DUDT(I:I,J:J,:),DVDT(I:I,J:J,:)          &
!!!  added for SAS-hurricane
                       ,SAS_MOMMIX,SAS_PGCON                                   &   !hwrf,namelist
                       ,SAS_MASS_FLUX                                          &   !hwrf,namelist
                       ,SAS_SHALCONV,SAS_SHAL_PGCON                            &   !hwrf,namelist
                       ,W_TOT(I:I,J:J,:),PSGDT(I:I,J:J,:)                      &
!!!  SAS-huricane
                       ,A2,A3,A4,CAPPA,CP,ELIV,ELWV,EPSQ,G,P608,PQ0,R_D,TIW    &
                       ,I,I,J,J,LM                                             &
                       ,I,I,J,J                                                &
                       ,I,I,J,J                                                &
                       ,I,I,J,J)
!
          ELSE
!
          STOP ! call abort
!
          ENDIF
!
!-----------------------------------------------------------------------
!
      DO L=1,LM
        u_phy(i,j,l)=u_phy(i,j,l)+DUDT(i,j,l)*DT*NPRECIP
        v_phy(i,j,l)=v_phy(i,j,l)+DVDT(i,j,l)*DT*NPRECIP
      ENDDO
!
!-----------------------------------------------------------------------
!
        ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Microphysics
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        IF(CALL_PRECIP)THEN
!
          CALL GSMDRIVE(NTIMESTEP,DT,NPRECIP                         &
                       ,SM(I:I,J:J),FIS(I:I,J:J)                     &
                       ,DSG2,SGML2,PDSG1,PSGML1                      &
                       ,PT,PD(I:I,J:J),T(I:I,J:J,:)                  &
                       ,Q(I:I,J:J,:),CW(I:I,J:J,:),OMGALF(I:I,J:J,:) &
                       ,TRAIN(I:I,J:J,:),SR(I:I,J:J)                 &
                       ,F_ICE(I:I,J:J,:),F_RAIN(I:I,J:J,:)           &
                       ,F_RIMEF(I:I,J:J,:)                           &
                       ,QC(I:I,J:J,:),QR(I:I,J:J,:)                  &
                       ,QI(I:I,J:J,:),QS(I:I,J:J,:),QG(I:I,J:J,:)    &
                       ,NI(I:I,J:J,:),NR(I:I,J:J,:)                  & ! G. Thompson
                       ,F_QC,F_QR,F_QI,F_QS,F_QG                     &
                       ,F_NI,F_NR                                    & ! G. Thompson
                       ,PREC(I:I,J:J),ACPREC(I:I,J:J),AVRAIN(I:I,J:J)&
                       ,ACPREC_TOT(I:I,J:J),acpcp_ra(I:I,J:J)        &
                       ,acpcp_sn(I:I,J:J),acpcp_gr(I:I,J:J)          & ! G. Thompson
                       ,refl_10cm(I:I,J:J,:),re_cloud(I:I,J:J,:)     &
                       ,re_ice(I:I,J:J,:),re_snow(I:I,J:J,:)         & !  G. Thompson
                       ,has_reqc,has_reqi,has_reqs                   & !  G. Thompson
                       ,MP_RESTART_STATE(:),TBPVS_STATE(:)           &
                       ,TBPVS0_STATE(:)                              &
                       ,MICROPHYSICS,RHGRD,TP1(I:I,J:J,:)            & !gfs mod-brad
                       ,QP1(I:I,J:J,:),PSP1(I:I,J:J),USE_RADAR       &
                       ,DFI_TTEN(I:I,J:J,:)                          &
                       ,I,I,J,J,LM                                   &
                       ,I,I,J,J                                      &
                       ,I,I,J,J                                      &
                       ,I,I,J,J,MPRATES(I:I,J:J,:,:),D_SS)
!
!-----------------------------------------------------------------------
!***  2 of 3 calls to CLTEND, calculate Tadj and replace T with Told
!-----------------------------------------------------------------------
!
        cld_tend2: IF(NPRECIP>1) THEN
          DO K=1,LM
            Tadj(I,J,K)=RNPRECIP*(T(I,J,K)-Told(I,J,K))
            T(I,J,K)=Told(I,J,K)
          ENDDO
        ENDIF  cld_tend2
!
!-----------------------------------------------------------------------
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  3 of 3 calls to CLTEND, incremental updates of T using Told & Tadj
!-----------------------------------------------------------------------
!
        cld_tend3: IF(NPRECIP>1) THEN
          DO K=1,LM
            T(I,J,K)=T(I,J,K)+Tadj(I,J,K)
          ENDDO
        ENDIF  cld_tend3
!
!-----------------------------------------------------------------------
!***  Prevent supersaturation w/r/t water and smooth temperature profiles
!     if lapse rates are steeper than dry adiabatic above lowest levels.
!-----------------------------------------------------------------------
!
        CALL TQADJUST(T(I:I,J:J,:)                  &
                     ,Q(I:I,J:J,:),QC(I:I,J:J,:)    &
                     ,CW(I:I,J:J,:),F_ICE(I:I,J:J,:)&
                     ,F_RAIN(I:I,J:J,:),PD(I:I,J:J) &
                     ,DSG2,PDSG1                    &
                     ,PSGML1,SGML2                  &
                     ,SPEC_ADV,RHGRD                &
                     ,LM,I,I,J,J)
!
      ENDDO !---JM
      ENDDO !---IM
!
!-----------------------------------------------------------------------
!
! ADD i,j,ntimestep,lm,mype everywhere
!
!-----------------------------------------------------------------------
!*** Update U and V (DUDT/DVDT from TURBL and CONVECTION)
!*** move from mass to wind points
!-----------------------------------------------------------------------
!
        IF(CALL_PRECIP)THEN
          DO L=1,LM
          DO J=JMS,JME
          DO I=IMS,IME
            U(i,j,l)=u_phy(i,j,l)
            V(i,j,l)=v_phy(i,j,l)
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!---- PHY_RUN END ------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!
 if ( mod(ntimestep,800)==0) then
        write(0,*)
        write(0,*)'NTSD= ',ntimestep
        write(0,*)'RR01 ',SIGT4(1,1)
        write(0,*)'RR02 ',RLWIN(1,1)
        write(0,*)'RR03 ',RSWIN(1,1)
        write(0,*)'RR04 ',u_phy(1,1,35)
        write(0,*)'RR05 ',RADOT(1,1)
        write(0,*)'RR06 ',SR(1,1)
        write(0,*)'RR07 ',TSKIN(1,1)
        write(0,*)'RR08 ',USTAR(1,1)
        write(0,*)'RR09 ',PBLH(1,1)
        write(0,*)'RR10 ',LPBL(1,1)
        write(0,*)'RR11 ',THZ0(1,1)
        write(0,*)'RR12 ',QZ0(1,1)
        write(0,*)'RR13 ',UZ0(1,1)
        write(0,*)'RR14 ',VZ0(1,1)
        write(0,*)'RR15 ',SMSTAV(1,1)
        write(0,*)'RR16 ',SMSTOT(1,1)
        write(0,*)'RR17 ',SSROFF(1,1)
        write(0,*)'RR18 ',BGROFF(1,1)
        write(0,*)'RR19 ',EPSR(1,1)
        write(0,*)'RR20 ',U10(1,1)
        write(0,*)'RR21 ',V10(1,1)
        write(0,*)'RR22 ',TH10(1,1)
        write(0,*)'RR23 ',Q10(1,1)
        write(0,*)'RR24 ',TSHLTR(1,1)
        write(0,*)'RR25 ',QSHLTR(1,1)
        write(0,*)'RR26 ',PSHLTR(1,1)
        write(0,*)'RR27 ',PSFC(1,1)
        write(0,*)'RR28 ',T2(1,1)
        write(0,*)'RR29 ',TWBS(1,1)
        write(0,*)'RR30 ',QWBS(1,1)
        write(0,*)'RR31 ',SFCSHX(1,1)
        write(0,*)'RR32 ',SFCLHX(1,1)
        write(0,*)'RR33 ',SFCEVP(1,1)
        write(0,*)'RR34 ',POTEVP(1,1)
        write(0,*)'RR35 ',POTFLX(1,1)
        write(0,*)'RR36 ',SUBSHX(1,1)
        write(0,*)'RR37 ',APHTIM(1,1)
        write(0,*)'RR38 ',ARDSW(1,1)
        write(0,*)'RR39 ',ARDLW(1,1)
endif
!
      END SUBROUTINE SOLVER_RUN
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      END MODULE MODULE_SOLVER_GRID_COMP
!
!-----------------------------------------------------------------------
