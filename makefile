SHELL = /bin/sh

include ./conf/configure.nmmb

LIBNMMBPHYS  = libnmmbphys.a
PHYS_EXE = phys_run.x

TARGET = $(PHYS_EXE)

#
# NMMB physics modules
#
MODULES_NMM_DRVS = module_PHYSICS_INIT.o          \
                   module_SOLVER_GRID_COMP.o      \
                   module_GWD.o                   \
                   module_CONVECTION.o            \
                   module_MICROPHYSICS.o          \
                   module_RADIATION.o             \
                   module_TURBULENCE.o

MODULES_NMM_PHYS = module_ESMF.o                  \
                   module_KINDS.o                 \
                   module_CONSTANTS.o             \
                   module_CONTROL.o               \
                   module_DIAGNOSE.o              \
                   module_BL_GFSPBL.o             \
                   module_BL_GFSPBLHUR.o          \
                   module_BL_GFSPBLEDMFHUR.o      \
                   module_BL_MYJPBL.o             \
                   module_CU_BMJ.o                \
                   module_CU_SAS.o                \
                   module_CU_SASHUR.o             \
                   module_CU_SCALE.o              \
                   module_LS_LISS.o               \
                   module_LS_NOAHLSM.o            \
                   module_MP_ETANEW.o             \
                   module_MP_FER_HIRES.o          \
                   module_mp_thompson.o           \
                   module_mp_radar.o              \
                   module_MP_GFS.o                \
                   module_MP_WSM6.o               \
                   module_RA_GFDL.o               \
                   module_RA_RRTM.o               \
                   module_SF_JSFC.o               \
                   module_SF_GFDL.o               \
                   module_SF_URBAN.o

MODULES_NMM_RRTM = grrad_nmmb.o                   \
                   radiation_astronomy_nmmb.o     \
                   radiation_aerosols_nmmb.o      \
                   radiation_gases_nmmb.o         \
                   radiation_clouds_nmmb.o        \
                   radiation_surface_nmmb.o       \
                   rad_initialize_nmmb.o          \
                   radlw_main_nmmb.o              \
                   radsw_main_nmmb.o              \
                   precpd_nmmb.o

#
# GFS physics modules
#
MODULES_GFS      = machine.o                      \
                   physparam.o                    \
                   physcons.o                     \
                   funcphys.o

MODULES_GFSF     = iounitdef.o                    \
                   ozne_def.o                     \
                   mersenne_twister.o             \
                   radlw_param.o                  \
                   radlw_datatb.o                 \
                   radsw_param.o                  \
                   radsw_datatb.o                 \
                   mfpbl.o                        \
                   moninedmf.o                    \
                   moninq.o                       \
                   mfdeepcnv.o                    \
                   mfshalcnv.o                    \
                   sascnvn.o                      \
                   shalcnv.o                      \
                   gscond.o

DEPEND_FILES = ${MODULES_GFS:.o=.f} \
               ${MODULES_GFSF:.o=.f} \
               ${MODULES_NMM_PHYS:.o=.F90} \
               ${MODULES_NMM_RRTM:.o=.f} \
               ${MODULES_NMM_DRVS:.o=.F90}

MODULES = $(MODULES_GFS) \
          $(MODULES_GFSF) \
          $(MODULES_NMM_PHYS) \
          $(MODULES_NMM_RRTM) \
          $(MODULES_NMM_DRVS)

all default: depend
	$(MAKE) $(LIBNMMBPHYS)
	$(MAKE) $(TARGET)

$(LIBNMMBPHYS): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?

$(PHYS_EXE): phys_run.o $(LIBNMMBPHYS)
	$(FC) $(FFLAGS_NMM) -o $@ $^

$(MODULES_GFS): %.o: %.f
	$(FC) $(FFLAGS_GFS) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

$(MODULES_GFSF): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

$(MODULES_NMM_PHYS): %.o: %.f90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c $*.f90

$(MODULES_NMM_RRTM): %.o: %.f
	$(FC) $(FREE) $(FFLAGS_NMM) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

$(MODULES_NMM_DRVS): %.o: %.f90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c $*.f90

phys_run.o: phys_run.f90 phys_run.F90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c phys_run.f90

.PHONY: clean
clean:
	@echo "Cleaning nmmbphys ... "
	@echo
	$(RM) -f $(LIBNMMBPHYS) $(PHYS_EXE) *.f90 *.o *.mod *.lst *.i depend

MKDEPENDS = ./mkDepends.pl

.SUFFIXES:
.SUFFIXES: .F90 .f90 .f .o .c
.F90.f90:
	$(CPP) $(CPPFLAGS) $< > $*.f90
.f.f90:
	$(CPP) $(CPPFLAGS) $< > $*.f90
depend: $(DEPEND_FILES)
	@echo "Building dependencies ..."
	@ls -1 $(DEPEND_FILES) > Srcfiles
	@echo "." > Filepath
	@$(MKDEPENDS) -m Filepath Srcfiles > depend
	@$(RM) -f Filepath Srcfiles

# do not include 'depend' file if the target contains string 'clean'
ifneq (clean,$(findstring clean,$(MAKECMDGOALS)))
    -include depend
endif

