!-----------------------------------------------------------------------
!
      MODULE module_PHYSYCS_INIT
!
!-----------------------------------------------------------------------
!
      USE MODULE_KINDS
      USE MODULE_CONSTANTS        ,ONLY : A2,A3,A4,CAPPA,CP,G,PQ0,DBZmin
      USE MODULE_RA_GFDL          ,ONLY : GFDL_INIT
      USE MODULE_RA_RRTM          ,ONLY : RRTM_INIT
      USE MODULE_TURBULENCE
      USE MODULE_SF_JSFC          ,ONLY : JSFC_INIT
      USE MODULE_BL_MYJPBL        ,ONLY : MYJPBL_INIT
      USE MODULE_LS_NOAHLSM       ,ONLY : NOAH_LSM_INIT,NUM_SOIL_LAYERS
      USE MODULE_CU_BMJ           ,ONLY : BMJ_INIT
      USE MODULE_CU_SAS           ,ONLY : SAS_INIT
      USE MODULE_CU_SASHUR        ,ONLY : SASHUR_INIT
      USE MODULE_CU_SCALE         ,ONLY : SCALECU_INIT
      USE MODULE_CONVECTION
      USE MODULE_MICROPHYSICS_NMM ,ONLY : MICRO_RESTART
      USE MODULE_MP_ETANEW        ,ONLY : FERRIER_INIT
      USE MODULE_MP_FER_HIRES     ,ONLY : FERRIER_INIT_HR
      USE MODULE_MP_WSM6          ,ONLY : WSM6INIT
      USE MODULE_MP_THOMPSON      ,ONLY : THOMPSON_INIT
      USE MODULE_MP_GFS           ,ONLY : GFSMP_INIT
      USE MODULE_GWD              ,ONLY : GWD_INIT
      USE FUNCPHYS
      USE MODULE_CONTROL          ,ONLY : CONSTS
      USE MODULE_MP_FER_HIRES     ,ONLY : GPVS_HR
      USE MODULE_CONSTANTS        ,ONLY : A,CLIQ,CV,DTR,PI,R,G &
                                         ,RHOAIR0,RHOWATER,RHOSNOW
      USE OZNE_DEF                ,ONLY: LEVOZC,LATSOZP,BLATC,TIMEOZC,TIMEOZ &
                                        ,LEVOZP,DPHIOZC,LATSOZC,PL_COEFF
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: PHYSYCS_INIT
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE PHYSYCS_INIT &
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
,NRDSW,NSRFC,CU_PHYSICS,JULYR,NSTEPS_PER_CHECK,NSTEPS_PER_RESET,IDAT,DT,PT)
!
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT) :: ISOL,ICO2,IALB,IEMS,IAER,ICTM,IOVR_SW,IOVR_LW
      INTEGER(kind=KINT) :: NTOZ,NTCW,SASHAL,RAS

      INTEGER(kind=KINT) :: NFCST,ISUBCSW,ISUBCLW,IAER_MDL    &
                           ,ICLIQ_SW,ICICE_SW,ICLIQ_LW,ICICE_LW,IFLIP
      REAL(kind=KFPT) :: GMT,DT_MICRO,TROG=2.*R/G,DZ,RG=1./G
      REAL(kind=KFPT),DIMENSION(:),ALLOCATABLE :: SFULL,SMID,SFULL_FLIP,SMID_FLIP
      REAL(KIND=KDBL),DIMENSION(:),ALLOCATABLE :: SFULLD
      LOGICAL(kind=KLOG) :: OPENED,LSASHAL,CRICK_PROOF,CCNORM,NORAD_PRECIP
      LOGICAL(kind=KLOG),SAVE :: ALLOWED_TO_READ=.TRUE.
      CHARACTER(LEN=256) :: INFILE
!
!-----------------------------------------------------------------------
!***  Variables from NAMELIST
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT) :: INTEGER_DT,NUMERATOR_DT,IDENOMINATOR_DT
!
      INTEGER(kind=KINT) :: START_YEAR,START_MONTH,START_DAY,START_HOUR &
                           ,START_MINUTE,START_SECOND,MINUTES_HISTORY
!
      LOGICAL(kind=KLOG) :: RESTART
      REAL(kind=KFPT)    :: AVG_MAX_LENGTH
      LOGICAL(kind=KLOG) :: RST_OUT_00
!
      LOGICAL(kind=KLOG) :: gwdflg
      REAL(KIND=KFPT)    :: CDMB,CLEFF,SIGFAC,factop,rlolev,dpmin
!
      CHARACTER(256)     :: LONGWAVE,SHORTWAVE,CONVECTION,MICROPHYSICS,TURBULENCE,SFC_LAYER,LAND_SURFACE
!
      INTEGER(kind=KINT) :: NP3D
      INTEGER(kind=KINT) :: IALBSRC
      CHARACTER(256)     :: CLDFRACTION
      INTEGER(kind=KINT) :: CO2TF
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
      INTEGER(kind=KINT) :: nhrs_prec,nhrs_heat,nhrs_clod,nhrs_rdlw,nhrs_rdsw,nhrs_srfc
!
      REAL(kind=KFPT)    :: sas_pgcon,sas_shal_pgcon,sas_shalconv,sas_mass_flux,sas_mommix &
                           ,var_ric,coef_ric_l,coef_ric_s,ALPHA,SFENTH
!
!-----------------------------------------------------------------------
!***  END Variables from NAMELIST
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Variables from internal state
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT),PARAMETER :: IME=1,IMS=1,JME=1,JMS=1,LM=35 !!!!rv
      INTEGER(kind=KINT)           :: D_SS
      LOGICAL(kind=KLOG)           :: F_QC,F_QR,F_QI,F_QS,F_QG,F_NI,F_NR,lmprate=.true.
      INTEGER(kind=KINT)           :: P_QC,P_QR,P_QI,P_QS,P_QG,P_NI,P_NR

      INTEGER(kind=KINT) :: has_reqc=0,has_reqi=0,has_reqs=0
      REAL(KIND=KFPT),DIMENSION(1:MICRO_RESTART) :: MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,NUM_SOIL_LAYERS) :: SMC,STC,SH2O

      REAL(KIND=KFPT),DIMENSION(:,:,:,:),ALLOCATABLE :: MPRATES
!
!-----------------------------------------------------------------------
!***  END Variables from internal state
!-----------------------------------------------------------------------
!
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM-1) :: PSGDT

      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME):: ZLO
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM):: Told,Tadj,F_ICE,F_RAIN,F_RIMEF,refl_10cm,Q2,OMGALF &
                     ,O3,Z,CW,Q,T,U,V,RLWTT,RSWTT,EXCH_H,XLEN_MIX,CLDFRA,TRAIN &
                     ,TCUCN,W_TOT,DUDT,DVDT &
                     ,re_cloud,re_ice,re_snow
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME,LM+1):: PINT
!---
      INTEGER(KIND=KINT),DIMENSION(IMS:IME,JMS:JME) :: NCOUNT,NCFRST,NCFRCV,ISLTYP,IVGTYP &
                                                      ,LPBL
      REAL(KIND=KFPT),DIMENSION(IMS:IME,JMS:JME) :: PDO,ACFRCV,ACFRST,AKHS,AKHS_OUT,AKMS &
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
                     ,GLAT,GLON,SI,TG

      REAL(KIND=KFPT),DIMENSION(LM-1) :: EPSL
      REAL(KIND=KFPT),DIMENSION(LM)   :: EPSQ2
      REAL(KIND=KFPT),DIMENSION(LM)   :: DSG2,SGML2,PSGML1,PDSG1
      REAL(KIND=KFPT),DIMENSION(LM+1) :: SG2,SGM,PSG1
      INTEGER(kind=KINT) :: NSTEPS_PER_HOUR,NPREC,NCLOD,NHEAT,NRDLW &
                           ,NRDSW,NSRFC
      INTEGER(kind=KINT) :: LSM_PHYSICS,CU_PHYSICS,MP_PHYSICS
      INTEGER(kind=KINT) :: MY_DOMAIN_ID=1,NUM_WATER=4
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER(kind=KINT) :: MPI_COMM_COMP,MYPE=0 !rv (MYPE for serial = 0)
!
      INTEGER(kind=KINT) :: I,J,K,KFLIP,L,N
!
      INTEGER(kind=KINT) :: JULYR,NSTEPS_PER_CHECK,NSTEPS_PER_RESET
!
      REAL(kind=KFPT) :: DUMMY
!
!-----------------------------------------------------------------------
!***  SAVEs are for dereferenced constant variables.
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT),DIMENSION(8)  :: IDAT
!
      REAL(kind=KFPT) :: DT,PT
!
      INTEGER :: KK
!
      namelist /configuration/ &
       INTEGER_DT,NUMERATOR_DT,IDENOMINATOR_DT &
      ,START_YEAR,START_MONTH,START_DAY,START_HOUR &
      ,START_MINUTE,START_SECOND,MINUTES_HISTORY &
      ,RESTART,AVG_MAX_LENGTH,RST_OUT_00 &
      ,gwdflg,CDMB,CLEFF,SIGFAC,factop,rlolev,dpmin &
      ,LONGWAVE,SHORTWAVE,CONVECTION,MICROPHYSICS &
      ,TURBULENCE,SFC_LAYER,LAND_SURFACE &
      ,NP3D,IALBSRC,CLDFRACTION,CO2TF &
      ,RHGRD,SPEC_ADV &
      ,UCMCALL,IVEGSRC &
      ,FRES,FR,FSL,FSS &
      ,ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP &
      ,NPHS,NRADS,NRADL,NPRECIP &
      ,nhrs_prec,nhrs_heat,nhrs_clod,nhrs_rdlw,nhrs_rdsw,nhrs_srfc &
      ,sas_pgcon,sas_shal_pgcon,sas_shalconv,sas_mass_flux,sas_mommix &
      ,var_ric,coef_ric_l,coef_ric_s,ALPHA,SFENTH
!
      open(127,file='configure_file')
      read(127,configuration)
print*,INTEGER_DT,NUMERATOR_DT,IDENOMINATOR_DT
print*,START_YEAR,START_MONTH,START_DAY,START_HOUR
print*,START_MINUTE,START_SECOND,MINUTES_HISTORY
print*,RESTART,AVG_MAX_LENGTH,RST_OUT_00
print*,CDMB,CLEFF,SIGFAC
print*,factop,rlolev,dpmin
print*,trim(SHORTWAVE)
print*,trim(CONVECTION)
print*,trim(MICROPHYSICS)
print*,trim(TURBULENCE)
print*,trim(SFC_LAYER)
print*,trim(LAND_SURFACE)
print*,NP3D,IALBSRC,trim(CLDFRACTION),CO2TF
print*,RHGRD,SPEC_ADV
print*,UCMCALL,IVEGSRC
print*,FRES,FR,FSL,FSS
print*,ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP
print*,NPHS,NRADS,NRADL,NPRECIP
print*,nhrs_prec,nhrs_heat,nhrs_clod,nhrs_rdlw,nhrs_rdsw,nhrs_srfc
print*,sas_pgcon,sas_shal_pgcon,sas_shalconv,sas_mass_flux,sas_mommix
print*,var_ric,coef_ric_l,coef_ric_s,ALPHA,SFENTH
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Ratko - START "internal state"
!-----------------------------------------------------------------------
!
      D_SS=1
      has_reqc=0
      has_reqi=0
      has_reqs=0

      NUM_WATER=0
      P_QC=0
      P_QI=0
      P_QR=0
      P_QS=0
      P_QG=0
      P_NI=0
      P_NR=0
      F_QC=.FALSE.
      F_QR=.FALSE.
      F_QS=.FALSE.
      F_QI=.FALSE.
      F_QG=.FALSE.
      F_NI=.FALSE.
      F_NR=.FALSE.

      IF(TRIM(MICROPHYSICS)=='fer'.OR. TRIM(MICROPHYSICS)=='fer_hires')THEN
        NUM_WATER=4
        P_QC=1
        P_QR=2
        P_QS=3
        P_QG=4
        F_QC=.TRUE.
        F_QR=.TRUE.
        F_QS=.TRUE.
        F_QG=.TRUE.
        if(lmprate) D_SS=24
      ELSEIF(TRIM(MICROPHYSICS)=='wsm6')THEN
        NUM_WATER=5
        P_QC=1
        P_QR=2
        P_QS=3
        P_QI=4
        P_QG=5
        F_QC=.TRUE.
        F_QR=.TRUE.
        F_QS=.TRUE.
        F_QI=.TRUE.
        F_QG=.TRUE.
        if(lmprate) D_SS=41
      ELSEIF(TRIM(MICROPHYSICS)=='thompson')THEN
        NUM_WATER=7
        P_QC=1
        P_QI=2
        P_QR=3
        P_QS=4
        P_QG=5
        P_NI=6
        P_NR=7
        F_QC=.TRUE.
        F_QR=.TRUE.
        F_QS=.TRUE.
        F_QI=.TRUE.
        F_QG=.TRUE.
        F_NI=.TRUE.
        F_NR=.TRUE.
        if(lmprate) D_SS=15
        IF(TRIM(LONGWAVE)=='rrtm'.AND.TRIM(SHORTWAVE)=='rrtm') THEN
           write(6,*) 'DEBUG-GT:  combined Thompson MP and RRTM radiation, therefore using coupled effective radii'
           has_reqc=1
           has_reqi=1
           has_reqs=1
        ELSE
           write(6,*) 'DEBUG-GT:  found Thompson MP but not RRTM radiation'
           write(6,*) '     this is not advised. Should use RRTM radiation'
           write(6,*) 'DEBUG-GT:  Long/short-wave set to: ', TRIM(LONGWAVE), TRIM(SHORTWAVE)
        ENDIF
      ELSEIF(TRIM(MICROPHYSICS)=='gfs')THEN
        NUM_WATER=2
        P_QC=1
        P_QI=2
        F_QC=.TRUE.
        F_QI=.TRUE.
      ELSE
        write(0,*) 'Unknown microphysics : ',TRIM(MICROPHYSICS)
        stop
      ENDIF

!allocate arrays from solver internal state
      IF(.NOT. ALLOCATED(MPRATES))  ALLOCATE(MPRATES(IMS:IME,JMS:JME,LM,D_SS))
print*,'EEE ',D_SS,TRIM(MICROPHYSICS)
!
!-----------------------------------------------------------------------
!***  Ratko - END "internal state"
!-----------------------------------------------------------------------
!
!rv  IF(NTIMESTEP==0 .or. RESTART) THEN !rv PHYS_INIT_PART
!
!-----------------------------------------------------------------------
!
        DT=REAL(INTEGER_DT)+REAL(NUMERATOR_DT)                &  !<-- Fundamental tiemstep (s) (REAL)
                                     /REAL(IDENOMINATOR_DT)
        NSTEPS_PER_RESET=NINT(AVG_MAX_LENGTH/DT)
        NSTEPS_PER_CHECK=MAX(2,NINT(40/DT))
!
!-----------------------------------------------------------------------
!
        DO J=JMS,JME
        DO I=IMS,IME
          PD(I,J)=0.
          PDO(I,J)=0.
        ENDDO
        ENDDO
!
        DO L=1,LM-1
        DO J=JMS,JME
        DO I=IMS,IME
          PSGDT(I,J,L)=0.
        ENDDO
        ENDDO
        ENDDO
!
!-- Initialize 4D microphysics rates (diagnostic arrays)
!
        DO KK=1,d_ss
          DO L=1,LM
            DO J=JMS,JME
            DO I=IMS,IME
              MPRATES(I,J,L,KK)=0.
            ENDDO
            ENDDO
          ENDDO
        ENDDO
!
!-- Initialize all "normal" 3D arrays
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          Told(I,J,L)=0.
          Tadj(I,J,L)=0.
          F_ICE(I,J,L)=0.
          F_RAIN(I,J,L)=0.
          F_RIMEF(I,J,L)=0.
          refl_10cm(I,J,L)=DBZmin
          Q2(I,J,L)=0.02
          OMGALF(I,J,L)=0.
          T(I,J,L)=-1.E6
          U(I,J,L)=-1.E6
          V(I,J,L)=-1.E6
          RLWTT(I,J,L)=0.
          RSWTT(I,J,L)=0.
          EXCH_H(I,J,L)=0.
          XLEN_MIX(I,J,L)=0.
          CLDFRA(I,J,L)=0.
          TRAIN(I,J,L) =0.
          TCUCN(I,J,L) =0.
          W_TOT(I,J,L)=0.
          DUDT(I,J,L)=0.
          DVDT(I,J,L)=0.
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,NUM_SOIL_LAYERS
        DO J=JMS,JME
        DO I=IMS,IME
          SMC(I,J,L)=-1.E6
          STC(I,J,L)=-1.E6
          SH2O(I,J,L)=-1.E6
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,MICRO_RESTART
          MP_RESTART_STATE(L)=0.
          TBPVS_STATE(L)=0.
          TBPVS0_STATE(L)=0.
        ENDDO
!
        DO J=JMS,JME
        DO I=IMS,IME
          LPBL(I,J)    =-999
          NCFRCV(I,J)  =-999
          NCFRST(I,J)  =-999
          ACFRCV(I,J)  =-1.E6
          ACFRST(I,J)  =-1.E6
          AKHS(I,J)    = 0.
          AKHS_OUT(I,J)= 0.
          AKMS(I,J)    = 0.
          AKMS_OUT(I,J)= 0.
          ALBASE(I,J)  =-1.E6
          ALBEDO(I,J)  =-1.E6
          ALBVB(I,J)   =-1.E6  ! MODIS visible+uv beam albedo
          ALBNB(I,J)   =-1.E6  ! MODIS near IR beam albedo
          ALBVD(I,J)   =-1.E6  ! MODIS visible+uv diffuse albedo
          ALBND(I,J)   =-1.E6  ! MODIS near IR diffuse albedo
          ALWIN(I,J)   =-1.E6
          ALWOUT(I,J)  =-1.E6
          ALWTOA(I,J)  =-1.E6
          ASWIN(I,J)   =-1.E6
          ASWOUT(I,J)  =-1.E6
          ASWTOA(I,J)  =-1.E6
          BGROFF(I,J)  =-1.E6
          CFRACH(I,J)  =-1.E6
          CFRACM(I,J)  =-1.E6
          CFRACL(I,J)  =-1.E6
          CNVBOT(I,J)  =0.0
          CNVTOP(I,J)  =0.0
          CMC(I,J)     =-1.E6
          CPRATE(I,J)  =0.0
          CUPPT(I,J)   =-1.E6
          CZMEAN(I,J)  =-1.E6
          CZEN(I,J)    =-1.E6
          EPSR(I,J)    =-1.E6
          FIS(I,J)     =-1.E6
          HBOT(I,J)    =-1.E6
          HBOTD(I,J)   =-1.E6
          HBOTS(I,J)   =-1.E6
          HTOP(I,J)    =-1.E6
          HTOPD(I,J)   =-1.E6
          HTOPS(I,J)   =-1.E6
          GRNFLX(I,J)  = 0.
          MAVAIL(I,J)  = 1.
          MXSNAL(I,J)  =-1.E6
          PBLH(I,J)    =-1.E6
          MIXHT(I,J)   =0.
          PD(I,J)      =-1.E6
          POTEVP(I,J)  = 0.
          POTFLX(I,J)  =-1.E6
          QSH(I,J)     = 0.
          QWBS(I,J)    =-1.E6
          QZ0(I,J)     = 0.
          RADOT(I,J)   = 0.
          RLWIN(I,J)   = 0.
          RMOL(I,J)    =-1.E6
          RSWIN(I,J)   = 0.
          RSWINC(I,J)  = 0.
          RSWOUT(I,J)  = 0.
          RLWTOA(I,J)  = 0.
          RSWTOA(I,J)  = 0.
          SFCEVP(I,J)  = 0.
          SFCEXC(I,J)  = 0.
          SFCLHX(I,J)  =-1.E6
          SFCSHX(I,J)  =-1.E6
          SICE(I,J)    =-1.E6
          SIGT4(I,J)   =-1.E6
          SM(I,J)      =-1.E6
          SMSTAV(I,J)  = 0.
          SMSTOT(I,J)  = 0.
          SNO(I,J)     = 0.
          SNOWC(I,J)   = 0.
          SNOPCX(I,J)  =-1.E6
          SOILTB(I,J)  = 273.
          SR(I,J)      =-1.E6
          SSROFF(I,J)  = 0.
          SST(I,J)     = 273.
          SUBSHX(I,J)  =-1.E6
          THS(I,J)     =-1.E6
          THZ0(I,J)    = 273.
          TSKIN(I,J)   =-1.E6
          TWBS(I,J)    =-1.E6
          USTAR(I,J)   = 0.1
          UZ0(I,J)     = 0.
          VEGFRC(I,J)  =-1.E6
          VZ0(I,J)     = 0.
          Z0(I,J)      =-1.E6
          Z0BASE(I,J)  =-1.E6
          STDH(I,J)    =-1.E6
          CROT(I,J)    = 0.
          SROT(I,J)    = 0.
          HSTDV(I,J)   = 0.
          HCNVX(I,J)   = 0.
          HASYW(I,J)   = 0.
          HASYS(I,J)   = 0.
          HASYSW(I,J)  = 0.
          HASYNW(I,J)  = 0.
          HLENW(I,J)   = 0.
          HLENS(I,J)   = 0.
          HLENSW(I,J)  = 0.
          HLENNW(I,J)  = 0.
          HANGL(I,J)   = 0.
          HANIS(I,J)   = 0.
          HSLOP(I,J)   = 0.
          HZMAX(I,J)   = 0.
        ENDDO
        ENDDO
!
        DO J=JMS,JME
        DO I=IMS,IME
          ACSNOM(I,J)= 0.
          ACSNOW(I,J)= 0.
          ACPREC(I,J)= 0.
          ACPREC_TOT(I,J)= 0.
          acpcp_ra(I,J)= 0.
          acpcp_sn(I,J)= 0.
          acpcp_gr(I,J)= 0.
          CUPREC(I,J)= 0.
          PREC(I,J)  = 0.
          CLDEFI(I,J)= 0.
          PSHLTR(I,J)= 1.E5
          PSFC(I,J)  = 1.E5
          Q10(I,J)   = 0.
          QSHLTR(I,J)= 0.
          T2(I,J)    = 273.
          TH10(I,J)  = 273.
          TSHLTR(I,J)= 273.
          U10(I,J)   = 0.
          V10(I,J)   = 0.
          TLMIN(I,J) = 0.
          TLMAX(I,J) = 0.

          ACUTIM(I,J)= 0.
          APHTIM(I,J)= 0.
          ARDLW(I,J) = 0.
          ARDSW(I,J) = 0.
          ASRFC(I,J) = 0.
          AVRAIN(I,J)= 0.
          AVCNVC(I,J)= 0.
        ENDDO
        ENDDO
!
        IF (has_reqc.eq.1 .and. has_reqi.eq.1 .and. has_reqs.eq.1) THEN
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          re_cloud(I,J,L)=2.51E-6
          re_ice(I,J,L)=10.1E-6
          re_snow(I,J,L)=20.1E-6
        ENDDO
        ENDDO
        ENDDO
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
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
          UPHLMAX(I,J)=-999.
          NCOUNT(I,J)=0
        ENDDO
        ENDDO
!
!--------------->>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<---------------!
!       CALL PHYSICS_INITIALIZE placeholder for INIT
!!!!!!!!!
!!!!!!!!! ---- read-in initial data ------------
!!!!!!!!!
open(7,file='INPUT.txt',form='formatted')
read(7,*)idat(1)
read(7,*)idat(2)
read(7,*)idat(3)
read(7,*)pt
do l=1,lm+1
read(7,*)sgm(l)
enddo
do l=1,lm+1
read(7,*)SG2(l)
enddo
do l=1,lm
read(7,*)DSG2(l)
enddo
do l=1,lm
read(7,*)SGML2(l)
enddo
do l=1,lm+1
read(7,*)PSG1(l)
enddo
do l=1,lm
read(7,*)PDSG1(l)
enddo
do l=1,lm
read(7,*)PSGML1(l)
enddo
do l=1,lm
read(7,*)EPSQ2(l)
enddo
do l=1,lm-1
read(7,*)EPSL(l)
enddo
read(7,*)fis(1,1)
read(7,*)stdh(1,1)
read(7,*)sm(1,1)
read(7,*)pd(1,1)
do l=1,lm
read(7,*)u(1,1,l)
enddo
do l=1,lm
read(7,*)v(1,1,l)
enddo
do l=1,lm
read(7,*)t(1,1,l)
enddo
do l=1,lm
read(7,*)q(1,1,l)
enddo
do l=1,lm
read(7,*)cw(1,1,l)
enddo
do l=1,lm
read(7,*)o3(1,1,l)
enddo
do l=1,lm+1
read(7,*)pint(1,1,l)
enddo
read(7,*)ALBEDO(1,1)
read(7,*)ALBASE(1,1)
read(7,*)EPSR(1,1)
read(7,*)MXSNAL(1,1)
read(7,*)TSKIN(1,1)
read(7,*)SST(1,1)
read(7,*)SNO(1,1)
read(7,*)SNOWC(1,1)
read(7,*)SI(1,1)
read(7,*)SICE(1,1)
read(7,*)TG(1,1)
read(7,*)CMC(1,1)
read(7,*)SR(1,1)
read(7,*)USTAR(1,1)
read(7,*)Z0(1,1)
read(7,*)Z0BASE(1,1)
do l=1,4
read(7,*)stc(1,1,l)
enddo
do l=1,4
read(7,*)smc(1,1,l)
enddo
do l=1,4
read(7,*)sh2o(1,1,l)
enddo
read(7,*)ISLTYP(1,1)
read(7,*)IVGTYP(1,1)
read(7,*)VEGFRC(1,1)
do l=1,4
read(7,*)!!!DZSOIL(l)
enddo
read(7,*)GLAT(1,1)
read(7,*)GLON(1,1)
close(7)
!!!!!!!!!
!!!!!!!!! ---- end read-in initial data --------
!!!!!!!!!
!--------------->>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<---------------!
!
!-----------------------------------------------------------------------
!
        CALL CONSTS(PT)
!
!-----------------------------------------------------------------------
!
      IF(.NOT. ALLOCATED(SMID)) THEN
        ALLOCATE(SMID(LM+1),SFULL(LM+1),SFULL_FLIP(LM+1),SMID_FLIP(LM+1),SFULLD(LM+1))
      ENDIF
!-----------------------------------------------------------------------
!***  Gravity wave drag (GWD) & mountain blocking (MB)
!
!***  Open and read GWD data file (14 orography fields)
!-----------------------------------------------------------------------
!
    gwd_read: IF(GWDFLG) THEN
 
      select_GWD_unit: DO N=51,59
        INQUIRE(N,OPENED=OPENED)
        IF(.NOT.OPENED)THEN
          NFCST=N
          EXIT select_GWD_unit
        ENDIF
      ENDDO select_GWD_unit
 
      WRITE(INFILE,'(A,I2.2)')'GWD_bin_',MY_DOMAIN_ID
 
!     CALL PHYSICS_READ_GWD(INFILE,NFCST,INT_STATE &
!                          ,MYPE,MPI_COMM_COMP &
!                          ,RC)
 
      ENDIF gwd_read
!
!----------------------------------------------------------------------
!****  GWD & MB initialization
!----------------------------------------------------------------------
!
!   DO J=JMS,JME
!   DO I=IMS,IME
!     CALL GWD_init(DT*NPHS,RESTART &
!                   ,CLEFFAMP,DPHD &
!                   ,CLEFF &
!                   ,TPH0D,TLM0D &
!                   ,GLAT(I:I,J:J),GLON(I:I,J:J) &
!                   ,CROT(I:I,J:J),SROT(I:I,J:J),HANGL(I:I,J:J) &
!                   ,I,I,J,J&
!                   ,I,I,J,J&
!                   ,I,I,J,J,LM)
!   ENDDO
!   ENDDO
!
!-----------------------------------------------------------------------
!***  Make up a potential skin temperature.
!-----------------------------------------------------------------------
!
      IF(.NOT.RESTART) THEN
!
        DO J=JMS,JME
        DO I=IMS,IME
          THS(I,J)=TSKIN(I,J)                       &
                       *(100000./(SG2(LM+1)*PD(I,J) &
                                 +PSG1(LM+1)))**CAPPA
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Calculate height
!-----------------------------------------------------------------------
!
      IF(.NOT.RESTART) THEN
!
        DO J=JMS,JME
        DO I=IMS,IME
          ZLO(I,J)=FIS(I,J)*RG
        DO L=LM,1,-1
!
          dz=(q(i,j,l)*0.608+(1.-cw(i,j,l)))*t(i,j,l)*trog &
             *(dsg2(l)*pd(i,j)+pdsg1(l))/(pint(i,j,l)+pint(i,j,l+1))
          z(i,j,l)=dz*0.5+zlo(i,j)
          zlo(i,j)=zlo(i,j)+dz
!
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!*** Initializing TLMAX, TLMIN
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
        DO I=IMS,IME
          TLMAX(I,J)=T(I,J,1)
          TLMIN(I,J)=T(I,J,1)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  Recreate sigma values at layer interfaces for the full vertical
!***  domain. 
!-----------------------------------------------------------------------
!
      DO L=1,LM+1
        SFULL(L)=SGM(L)
      ENDDO
!
      DO L=1,LM
        SMID(L)=(SFULL(L)+SFULL(L+1))*0.5
      ENDDO
!
      SMID(LM+1)=-9999999.
!
!-----------------------------------------------------------------------
!***  Set time variables needed for history output.
!-----------------------------------------------------------------------
!
      NSTEPS_PER_HOUR=NINT(3600./DT)
      NPREC=NSTEPS_PER_HOUR*NHRS_PREC
      NCLOD=NSTEPS_PER_HOUR*NHRS_CLOD
      NHEAT=NSTEPS_PER_HOUR*NHRS_HEAT
      NRDLW=NSTEPS_PER_HOUR*NHRS_RDLW
      NRDSW=NSTEPS_PER_HOUR*NHRS_RDSW
      NSRFC=NSTEPS_PER_HOUR*NHRS_SRFC
!
!-----------------------------------------------------------------------
!***  If this is a restarted run from timestep 0 then zero out
!***  the accumulated precip since they pass through the analysis
!***  with nonzero values from the first guess.
!-----------------------------------------------------------------------
!
      IF(RST_OUT_00)THEN
        DO J=JMS,JME
        DO I=IMS,IME
          ACPREC(I,J)=0.
          ACPREC_TOT(I,J)=0.
          CUPREC(I,J)=0.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!***  Finally initialize individual schemes.
!-----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  Longwave radiation
!----------------------------------------------------------------------
!
        SELECT CASE (longwave)  
          CASE ('gfdl')
!
!***  We are calling a WRF routine thus flip the vertical.
!
            DO K=1,LM
              KFLIP=LM+1-K
              SFULL_FLIP(KFLIP)=SFULL(K+1)
              SMID_FLIP(KFLIP)=SMID(K)
            ENDDO
            SFULL_FLIP(LM+1)=SFULL(1)
!
            GMT=REAL(START_HOUR)
            CALL GFDL_INIT(SFULL_FLIP,SMID_FLIP,PT*1.0E-3                  &
                          ,JULYR,START_MONTH,START_DAY,GMT             &
                          ,CO2TF,LM)
          CASE ('rrtm')

            CALL GPKAP    ! for ozone by using the unified RRTM from GFS
            CALL GPVS     ! for aerosol by using the unified RRTM from GFS
            CALL GPVS_HR  !- Initialize regional version of FPVS, FPVS0 functions
!
!-----------------------------------------------------------------------
!***  For threading safe  (rad_initialize). Default value
!-----------------------------------------------------------------------
!
            ICTM=1     !  0: use data at initial cond time, if not available, use latest, no extrapolation.
                       !  1: use data at the forecast time, if not available, use latest and extrapolation.
                       ! -1: use user provided external data for the fcst time, no extrapolation.
                       ! -2: same as ictm=0, but add seasonal cycle from climatology. no extrapolation.
                       ! yyyy0: use yyyy data for the forecast time, no further data extrapolation.
                       ! yyyy1: use yyyy data for the fcst. if needed, do extrapolation to match the fcst time.
            ISOL=0     ! 0: use a fixed solar constant value 1.3660e+3 (default)
                       !10: use a fixed solar constant value 1.3608e+3
                       ! 1: use 11-year cycle solar constant table
            ICO2=1     ! 0: use prescribed global mean co2   (default)
                       ! 1: use observed co2 annual mean value only
                       ! 2: use obs co2 monthly data with 2-d variation
            IAER=11    ! flag for aerosols scheme selection (all options work for NMMB)
                       ! - 3-digit aerosol flag (volc,lw,sw)
                       !   0: turn all aeros effects off (sw,lw,volc)
                       !   1: use clim tropspheric aerosol for sw only
                       !  10: use clim tropspheric aerosol for lw only
                       !  11: use clim tropspheric aerosol for both sw and lw
                       ! 100: volc aerosol only for both sw and lw
                       ! 101: volc and clim trops aerosol for sw only
                       ! 110: volc and clim trops aerosol for lw only
                       ! 111: volc and clim trops aerosol for both sw and lw
                       !   2: gocart/BSC-Dust tropspheric aerosol for sw only
                       !  20: gocart/BSC-Dust tropspheric aerosol for lw only
                       !  22: gocart/BSC-Dust tropspheric aerosol for both sw and lw
                       ! 102: volc and gocart trops aerosol for sw only
                       ! 120: volc and gocart trops aerosol for lw only
                       ! 122: volc and gocart trops aerosol for both sw and lw
            IAER_MDL=0 !  default aerosol model is opac-climatology
                       !  > 0,  future gocart-clim/prog scheme (not ready)
            IALB=2     ! control flag for surface albedo schemes
                       ! 0: climatology, based on surface veg types  ! ONLY THIS ONE WORKS (GFS)
                       ! 1: modis retrieval based surface albedo scheme
                       ! 2: use externally provided albedoes directly. ! ONLY THIS ONE WORKS for regional
                       !    (CALCULATES ALBEDO FROM NMMB MONTHLY CLIMATOLOGY AS IN GFDL RADIATION)
            IEMS=0     ! control flag for surface emissivity schemes
                       ! 0: fixed value of 1.0   (default)
                       ! 1: varying value based on surface veg types
            NTCW=3     !  0: no cloud condensate calculated
                       ! >0: array index location for cloud condensate
          ! NP3D=3     ! 3: ferrier's microphysics cloud scheme (only stratiform cloud)
                       !    (set iflagliq>0 in radsw_param.f and radlw_param.f)
                       ! 4: zhao/carr/sundqvist microphysics cloud (now available in the NMMB)
                       ! 5: NAM stratiform + convective cloud optical depth and fraction
                       !    (set iflagliq=0 in radsw_param.f and radlw_param.f)
            NTOZ=0     !  0: climatological ozone profile
                       ! >0: interactive ozone profile
            IOVR_SW=1  !  0 sw: random overlap clouds
                       !  1 sw: max-random overlap clouds
            IOVR_LW=1  !  0 lw: random overlap clouds
                       !  1 lw: max-random overlap clouds
            ISUBCSW=0  !  isubcsw/isubclw
                       !  sub-column cloud approx control flag (sw/lw rad)
                       !  0: with out sub-column cloud approximation
                       !  1: mcica sub-col approx. prescribed random seed
                       !  2: mcica sub-col approx. provided random seed
            ISUBCLW=0

            !----------------------------------------------------------
            ! --- check physparam for detail of the following ---------

            ICLIQ_SW=1 ! sw optical property for liquid clouds
            ICICE_SW=3 ! sw optical property for ice clouds (only iswcliq>0)
            ICLIQ_LW=1 ! lw optical property for liquid clouds
            ICICE_LW=1 ! lw optical property for ice clouds (only ilwcliq>0)

            !----------------------------------------------------------

            IFLIP=0    !   0: input data from toa to sfc
                       !   1: input data from sfc to toa

            SASHAL=0              ! New Massflux based shallow convection  (Not in use for NMMB)
            LSASHAL=.false.
            if (SASHAL>0 .and. .not.RAS) LSASHAL=.true.
            CRICK_PROOF=.false.   ! flag for eliminating CRICK (smooths profiles)
            CCNORM=.true.         ! flag for incloud condensate mixing ratio
            NORAD_PRECIP=.false.  ! flag for precip in radiation
                                  ! .true. snow/rain has no impact on radiation

!-----------------------------------------------------------------------
!***  Initialize ozone
!-----------------------------------------------------------------------

!OZONE CLIMATOLOGY
!
! there is no header in global_o3clim.txt file

            IF (NTOZ .LE. 0) THEN     ! DIAGNOSTIC OZONE, ONLY THIS ONE WORKS
               LEVOZC  = 17
               LATSOZC = 18
               BLATC   = -85.0
               TIMEOZC = 12            !!!  this is not in header
               LATSOZP   = 2
               LEVOZP    = 1
               TIMEOZ    = 1
               PL_COEFF  = 0
            ENDIF

            DPHIOZC = -(BLATC+BLATC)/(LATSOZC-1)

!-----------------------------------------------------------------------
!***  End initialization  of ozone
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Use optional MODIS albedos
!-----------------------------------------------------------------------

            IF (IALBSRC > 0) THEN
               IALB=1

!-- Update ALBASE, SNOWC, and ALBEDO
               DO J=JMS,JME
               DO I=IMS,IME
!
!-- Initial snow fractions set to 1 and albedos set to 0.65 over sea ice
!
        albedos: IF (SICE(I,J)>0.5) THEN
!-- Sea ice:
                     SNOWC(I,J)=1.
                     ALBASE(I,J)=0.65
                     ALBEDO(I,J)=0.65

                 ELSEIF (SM(I,J)<0.5) THEN albedos
!
!-- Land: set the base albedo to be the average of the visible and near IR diffuse albedos
!
                     DUMMY=0.5*(ALBVD(I,J)+ALBND(I,J))
                     ALBASE(I,J)=MAX(0.01, DUMMY)

                     IF (SNO(I,J)<=0.) THEN
!-- No snow over land:
                        SNOWC(I,J)=0.
                        ALBEDO(I,J)=ALBASE(I,J)
                     ELSE
!-- Snow over land:
                        SNOWC(I,J)=1.
                        DUMMY=MXSNAL(I,J)-ALBASE(I,J)
                        ALBEDO(I,J)=ALBASE(I,J)+SNOWC(I,J)*DUMMY
                     ENDIF

                 ENDIF  albedos

!-- Leave the albedos over water unchanged

               ENDDO
               ENDDO
            ENDIF

!-----------------------------------------------------------------------

            DO L=1,LM+1
              SFULLD(L)=SFULL(L)    !-- double precision
            ENDDO

!==========================================================================
!  Similar to GFS "GFS_Initialize_ESMFMod.f" line #1103
!==========================================================================

!..Special case for altering microphysics coupling with RRTM radiation
!.. based on namelist settings.  The NP3Dx variable is incredibly convoluted
!.. and renamed many times, including icmphys, np3d, and num_p3d.  Extremely
!.. confusing and hard-wired and needs help to adapt to new physics couplings
!.. and choices for full flexibility.   G. Thompson 06Feb2013

!..SPECIAL TEST FOR THOMPSON MICROPHYSICS AND RRTM RADIATION.  It is strongly
!.. advised against using GFDL or other radiation in combination with Thompson
!.. microphysics because other schemes are not properly using the cloud data.

            IF (TRIM(SHORTWAVE)=='rrtm' .AND.                 &
     &          TRIM(MICROPHYSICS)=='thompson' ) THEN

              IF (NP3D /=8) THEN
                 WRITE(0,*)' User selected np3d=',NP3D
                 WRITE(0,*)' NP3D=8 for RRTM & THOMPSON MICROPHYSICS'
                 STOP ! call abort
              ENDIF

              ICICE_SW=4
              ICICE_LW=4

            ENDIF

!==========================================================================
!..For GFDL type diagnostic
!==========================================================================

            IF (NP3D == 5) THEN
              ICLIQ_SW=0
              ICLIQ_LW=0
            ENDIF

!==========================================================================

            call rad_initialize_nmmb                                   &
!        ---  inputs:
     &       ( SFULLD,LM,ICTM,ISOL,ICO2,IAER,IAER_MDL,IALB,IEMS,NTCW,  &
     &         NP3D,NTOZ,IOVR_SW,IOVR_LW,ISUBCSW,ISUBCLW,    &
     &         ICLIQ_SW,ICICE_SW,ICLIQ_LW,ICICE_LW,                    &
     &         LSASHAL,CRICK_PROOF,CCNORM,NORAD_PRECIP,IFLIP,MYPE )
!  ---        outputs:
!                ( none )

!==========================================================================
!==========================================================================

            DO K=1,LM
              KFLIP=LM+1-K
              SFULL_FLIP(KFLIP)=SFULL(K+1)
              SMID_FLIP(KFLIP)=SMID(K)
            ENDDO
            SFULL_FLIP(LM+1)=SFULL(1)
!
            GMT=REAL(START_HOUR)

!==========================================================================
! This following "RRTM_INIT" is only a L,M,H  DIAGNOSTIC cloud.
! It is not a real RRTM initialization
!==========================================================================


            CALL RRTM_INIT(SFULL_FLIP,SMID_FLIP,PT*1.0E-3                  &
                          ,JULYR,START_MONTH,START_DAY,GMT             &
                          ,CO2TF,LM)
!
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF LONGWAVE SCHEME: INIT '
        END SELECT
!
!----------------------------------------------------------------------
!***  Shortwave radiation
!----------------------------------------------------------------------
!
        SELECT CASE (shortwave)
          CASE ('gfdl')
          CASE ('rrtm')
          CASE ('dudh')
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF SHORTWAVE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Surface layer
!----------------------------------------------------------------------
!
        SELECT CASE (sfc_layer)
          CASE ('myj')
            IF(.NOT.RESTART)THEN
              DO J=JMS,JME
              DO I=IMS,IME
                USTAR(I,J)=0.1
              ENDDO
              ENDDO
            ENDIF
            CALL JSFC_INIT

          CASE ('gfdl')
            IF(.NOT.RESTART)THEN
              DO J=JMS,JME
              DO I=IMS,IME
                USTAR(I,J)=0.1
              ENDDO
              ENDDO
            ENDIF
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF SURFACE LAYER SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Turbulence
!----------------------------------------------------------------------
!
        SELECT CASE (turbulence)
          CASE ('myj')
            IF(.NOT.RESTART)THEN
              DO K=1,LM
              DO J=JMS,JME
              DO I=IMS,IME
                EXCH_H(I,J,K)=0.
              ENDDO
              ENDDO
              ENDDO
            ENDIF
            CALL MYJPBL_INIT

          CASE ('gfs')
          CASE ('gfshur')
          CASE ('gfsedmfhur')
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF TURBULENCE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Land surface
!----------------------------------------------------------------------
!
        SELECT CASE (land_surface)
          CASE ('noah')
            LSM_PHYSICS=LSMSCHEME
            CALL NOAH_LSM_INIT(IVEGSRC,MYPE,MPI_COMM_COMP)

          CASE ('liss')
            LSM_PHYSICS=LISSSCHEME

          CASE ('gfdlslab')
            LSM_PHYSICS=GFDLSLABSCHEME

          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF LAND SURFACE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!****  Convection
!----------------------------------------------------------------------
!
        SELECT CASE (convection)
          CASE ('bmj')
            CU_PHYSICS=BMJSCHEME
            IF(.NOT.RESTART)THEN
              DO J=JMS,JME
              DO I=IMS,IME
                CLDEFI(I,J)=0.6
              ENDDO
              ENDDO
            ENDIF
            CALL BMJ_INIT(a2,a3,a4,cappa,cp,pq0)

          CASE ('sas')
            CU_PHYSICS=SASSCHEME
            CALL SAS_INIT
!
          CASE ('sashur')
            CU_PHYSICS=SASHURSCHEME
            CALL SASHUR_INIT
!
          CASE ('scalecu')
            CU_PHYSICS=SCALECUSCHEME
            DO K=1,LM
            DO J=JMS,JME
            DO I=IMS,IME
              DUDT(I,J,K)=0.
              DVDT(I,J,K)=0.
            ENDDO
            ENDDO
            ENDDO
            CALL SCALECU_INIT
!
          CASE ('none')
!           WRITE(0,*)' User has chosen to run with no parameterized convection.'
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF CONVECTION SCHEME: INIT'
            WRITE(0,*)' User selected CONVECTION = ',TRIM(CONVECTION)
            STOP ! call abort
        END SELECT
!
!----------------------------------------------------------------------
!***  Microphysics
!----------------------------------------------------------------------
!
        SELECT CASE (microphysics)
!
          CASE ('fer')
            MP_PHYSICS=95
            DT_MICRO=NPRECIP*DT
!
            IF(.NOT.RESTART)THEN
              DO K=1,LM
              DO J=JMS,JME
              DO I=IMS,IME
                F_ICE(I,J,K)=0.
                F_RAIN(I,J,K)=0.
                F_RIMEF(I,J,K)=1.
              ENDDO
              ENDDO
              ENDDO
            ENDIF
            CALL FERRIER_INIT(DT_MICRO,DT,RESTART            &
                             ,MP_RESTART_STATE               &
                             ,TBPVS_STATE                    &
                             ,TBPVS0_STATE                   &
                             ,ALLOWED_TO_READ                &
                             ,MPI_COMM_COMP,MYPE)
!
          CASE ('fer_hires')
            MP_PHYSICS=5
            DT_MICRO=NPRECIP*DT
            IF(.NOT.RESTART)THEN
              DO K=1,LM
              DO J=JMS,JME
              DO I=IMS,IME
                F_ICE(I,J,K)=0.
                F_RAIN(I,J,K)=0.
                F_RIMEF(I,J,K)=1.
              ENDDO
              ENDDO
              ENDDO
            ENDIF
!
            CALL FERRIER_INIT_HR(DT_MICRO,DT,RESTART            &
                                ,MP_RESTART_STATE               &
                                ,TBPVS_STATE                    &
                                ,TBPVS0_STATE                   &
                                ,ALLOWED_TO_READ                &
                                ,MPI_COMM_COMP,MYPE)
!
          CASE ('gfs')
            MP_PHYSICS=99
            CALL GFSMP_INIT
!
          CASE ('wsm6')
            MP_PHYSICS=6
            CALL WSM6INIT(RHOAIR0,RHOWATER,RHOSNOW,CLIQ,CV                &
                         ,ALLOWED_TO_READ )
!
          CASE ('thompson')
            MP_PHYSICS=8
            CALL thompson_init()
!
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF MICROPHYSICS SCHEME: INIT'
            WRITE(0,*)' User selected MICROPHYSICS = ',TRIM(MICROPHYSICS)
            STOP ! call abort

        END SELECT
!
!-----------------------------------------------------------------------
! ENDIF ! PHYS_INIT_PART (NTIMESTEP==0 .or. RESTART)
!
!
      END SUBROUTINE PHYSYCS_INIT
!
!----------------------------------------------------------------------
!######################################################################
!-----------------------------------------------------------------------

      END MODULE module_PHYSYCS_INIT
!
!-----------------------------------------------------------------------
