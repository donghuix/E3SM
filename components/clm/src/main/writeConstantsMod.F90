module writeConstantsMod

contains

  subroutine writeConstants()
    use fileio_mod, only : fio_open, fio_close
    use pftvarcon
    use soilorder_varcon
    use clm_varcon
    use clm_varpar
    use AllocationMod
    use PhenologyMod

    implicit none
    integer :: fid = 23
    character(len=256) :: ofile = "E3SM_constants.txt"

    call fio_open(fid, ofile, 2)

    !clm_varpar
    write(fid, "(A)") "nlevsoi  "
    write(fid,*) nlevsoi
    write(fid, "(A)") "nlevsoifl"
    write(fid,*) nlevsoifl
    write(fid, "(A)") "nlevgrnd "
    write(fid,*) nlevgrnd
    write(fid,"(A)") "nlevurb        "
    write(fid,*) nlevurb
    write(fid,"(A)") "nlevlak        "
    write(fid,*) nlevlak
    write(fid,"(A)") "nlevdecomp     "
    write(fid,*) nlevdecomp
    write(fid,"(A)") "nlevdecomp_full"
    write(fid,*) nlevdecomp_full
    write(fid,"(A)") "nlevtrc_soil"
    write(fid,*) nlevtrc_soil
    write(fid,"(A)") "nlevtrc_full"
    write(fid,*) nlevtrc_full

    !from pftconrd
    write(fid, "(A)") "dleaf    "
    write(fid,*) dleaf
    write(fid, "(A)") "c3psn    "
    write(fid,*) c3psn
    write(fid, "(A)") "xl       "
    write(fid,*) xl
    write(fid, "(A)") "rhol     "
    write(fid,*) rhol
    write(fid, "(A)") "rhos     "
    write(fid,*) rhos
    write(fid, "(A)") "taul     "
    write(fid,*) taul
    write(fid, "(A)") "taus     "
    write(fid,*) taus
    write(fid, "(A)") "z0mr     "
    write(fid,*) z0mr
    write(fid, "(A)") "displar  "
    write(fid,*) displar
    write(fid, "(A)") "roota_par"
    write(fid,*) roota_par
    write(fid, "(A)") "rootb_par"
    write(fid,*) rootb_par
    write(fid, "(A)") "crop     "
    write(fid,*) crop
    write(fid, "(A)") "irrigated"
    write(fid,*) irrigated
    write(fid, "(A)") "smpso    "
    write(fid,*) smpso
    write(fid, "(A)") "smpsc    "
    write(fid,*) smpsc
    write(fid, "(A)") "fnitr    "
    write(fid,*) fnitr
    write(fid, "(A)") "slatop   "
    write(fid,*) slatop
    write(fid, "(A)") "dsladlai "
    write(fid,*) dsladlai
    write(fid, "(A)") "leafcn   "
    write(fid,*) leafcn
    write(fid, "(A)") "flnr     "
    write(fid,*) flnr
    write(fid, "(A)") "woody    "
    write(fid,*) woody
    write(fid, "(A)") "lflitcn  "
    write(fid,*) lflitcn
    write(fid, "(A)") "frootcn  "
    write(fid,*) frootcn
    write(fid, "(A)") "livewdcn "
    write(fid,*) livewdcn
    write(fid, "(A)") "deadwdcn "
    write(fid,*) deadwdcn

    write(fid,"(A)") "leafcp       "
    write(fid,*) leafcp
    write(fid,"(A)") "lflitcp      "
    write(fid,*) lflitcp
    write(fid,"(A)") "frootcp      "
    write(fid,*) frootcp
    write(fid,"(A)") "livewdcp     "
    write(fid,*) livewdcp
    write(fid,"(A)") "deadwdcp     "
    write(fid,*) deadwdcp
    write(fid,"(A)") "grperc       "
    write(fid,*) grperc
    write(fid,"(A)") "grpnow       "
    write(fid,*) grpnow
    write(fid,"(A)") "rootprof_beta"
    write(fid,*) rootprof_beta
    write(fid,"(A)") "graincn      "
    write(fid,*) graincn
    write(fid,"(A)") "graincp      "
    write(fid,*) graincp
    write(fid,"(A)") "mxtmp        "
    write(fid,*) mxtmp
    write(fid,"(A)") "baset        "
    write(fid,*) baset
    write(fid,"(A)") "declfact     "
    write(fid,*) declfact
    write(fid,"(A)") "bfact        "
    write(fid,*) bfact
    write(fid,"(A)") "aleaff       "
    write(fid,*) aleaff
    write(fid,"(A)") "arootf       "
    write(fid,*) arootf
    write(fid,"(A)") "astemf       "
    write(fid,*) astemf
    write(fid,"(A)") "arooti       "
    write(fid,*) arooti
    write(fid,"(A)") "fleafi       "
    write(fid,*) fleafi
    write(fid,"(A)") "allconsl     "
    write(fid,*) allconsl
    write(fid,"(A)") "allconss     "
    write(fid,*) allconss
    write(fid,"(A)") "ztopmx       "
    write(fid,*) ztopmx
    write(fid,"(A)") "laimx        "
    write(fid,*) laimx
    write(fid,"(A)") "gddmin       "
    write(fid,*) gddmin
    write(fid,"(A)") "hybgdd       "
    write(fid,*) hybgdd
    write(fid,"(A)") "lfemerg      "
    write(fid,*) lfemerg
    write(fid,"(A)") "grnfill      "
    write(fid,*) grnfill
    write(fid,"(A)") "mxmat        "
    write(fid,*) mxmat
    write(fid,"(A)") "mnNHplantdate"
    write(fid,*) mnNHplantdate
    write(fid,"(A)") "mxNHplantdate"
    write(fid,*) mxNHplantdate
    write(fid,"(A)") "mnSHplantdate"
    write(fid,*) mnSHplantdate
    write(fid,"(A)") "mxSHplantdate"
    write(fid,*) mxSHplantdate
    write(fid,"(A)") "planttemp    "
    write(fid,*) planttemp
    write(fid,"(A)") "minplanttemp "
    write(fid,*) minplanttemp
    write(fid,"(A)") "froot_leaf   "
    write(fid,*) froot_leaf
    write(fid,"(A)") "stem_leaf    "
    write(fid,*) stem_leaf
    write(fid,"(A)") "croot_stem   "
    write(fid,*) croot_stem
    write(fid,"(A)") "flivewd      "
    write(fid,*) flivewd
    write(fid,"(A)") "fcur         "
    write(fid,*) fcur
    write(fid,"(A)") "lf_flab      "
    write(fid,*) lf_flab
    write(fid,"(A)") "lf_fcel      "
    write(fid,*) lf_fcel
    write(fid,"(A)") "lf_flig      "
    write(fid,*) lf_flig
    write(fid,"(A)") "fr_flab      "
    write(fid,*) fr_flab
    write(fid,"(A)") "fr_fcel      "
    write(fid,*) fr_fcel
    write(fid,"(A)") "fr_flig      "
    write(fid,*) fr_flig
    write(fid,"(A)") "leaf_long    "
    write(fid,*) leaf_long
    write(fid,"(A)") "froot_long   "
    write(fid,*) froot_long
    write(fid,"(A)") "evergreen    "
    write(fid,*) evergreen
    write(fid,"(A)") "stress_decid "
    write(fid,*) stress_decid
    write(fid,"(A)") "season_decid "
    write(fid,*) season_decid
    write(fid,"(A)") "pconv        "
    write(fid,*) pconv
    write(fid,"(A)") "pprod10      "
    write(fid,*) pprod10
    write(fid,"(A)") "pprod100     "
    write(fid,*) pprod100
    write(fid,"(A)") "pprodharv10  "
    write(fid,*) pprodharv10
    write(fid,"(A)") "cc_leaf      "
    write(fid,*) cc_leaf
    write(fid,"(A)") "cc_lstem     "
    write(fid,*) cc_lstem
    write(fid,"(A)") "cc_dstem     "
    write(fid,*) cc_dstem
    write(fid,"(A)") "cc_other     "
    write(fid,*) cc_other
    write(fid,"(A)") "fm_leaf      "
    write(fid,*) fm_leaf
    write(fid,"(A)") "fm_lstem     "
    write(fid,*) fm_lstem
    write(fid,"(A)") "fm_dstem     "
    write(fid,*) fm_dstem
    write(fid,"(A)") "fm_other     "
    write(fid,*) fm_other
    write(fid,"(A)") "fm_root      "
    write(fid,*) fm_root
    write(fid,"(A)") "fm_lroot     "
    write(fid,*) fm_lroot
    write(fid,"(A)") "fm_droot     "
    write(fid,*) fm_droot
    write(fid,"(A)") "fsr_pft      "
    write(fid,*) fsr_pft
    write(fid,"(A)") "fd_pft       "
    write(fid,*) fd_pft
    write(fid,"(A)") "fertnitro    "
    write(fid,*) fertnitro
    write(fid,"(A)") "fleafcn      "
    write(fid,*) fleafcn
    write(fid,"(A)") "ffrootcn     "
    write(fid,*) ffrootcn
    write(fid,"(A)") "fstemcn      "
    write(fid,*) fstemcn
    write(fid,"(A)") "presharv     "
    write(fid,*) presharv
    write(fid,"(A)") "convfact     "
    write(fid,*) convfact
    write(fid,"(A)") "fyield       "
    write(fid,*) fyield
    write(fid,"(A)") "root_dmx     "
    write(fid,*) root_dmx

    write(fid,"(A)") "VMAX_PLANT_NH4"
    write(fid,*) VMAX_PLANT_NH4
    write(fid,"(A)") "VMAX_PLANT_NO3"
    write(fid,*) VMAX_PLANT_NO3
    write(fid,"(A)") "VMAX_PLANT_P  "
    write(fid,*) VMAX_PLANT_P
    write(fid,"(A)") "VMAX_MINSURF_P_vr"
    write(fid,*) VMAX_MINSURF_P_vr
    write(fid,"(A)") "KM_PLANT_NH4"
    write(fid,*) KM_PLANT_NH4
    write(fid,"(A)") "KM_PLANT_NO3"
    write(fid,*) KM_PLANT_NO3
    write(fid,"(A)") "KM_PLANT_P  "
    write(fid,*) KM_PLANT_P
    write(fid,"(A)") "KM_MINSURF_P_vr    "
    write(fid,*) KM_MINSURF_P_vr
    write(fid,"(A)") "decompmicc_patch_vr"
    write(fid,*) decompmicc_patch_vr
    write(fid,"(A)") "VMAX_PTASE "
    write(fid,*) VMAX_PTASE
    write(fid,"(A)") "i_vc       "
    write(fid,*) i_vc
    write(fid,"(A)") "s_vc       "
    write(fid,*) s_vc
    write(fid,"(A)") "alpha_nfix "
    write(fid,*) alpha_nfix
    write(fid,"(A)") "alpha_ptase"
    write(fid,*) alpha_ptase
    write(fid,"(A)") "ccost_nfix "
    write(fid,*) ccost_nfix
    write(fid,"(A)") "pcost_nfix "
    write(fid,*) pcost_nfix
    write(fid,"(A)") "ccost_ptase"
    write(fid,*) ccost_ptase
    write(fid,"(A)") "ncost_ptase"
    write(fid,*) ncost_ptase
    write(fid,"(A)") "VMAX_NFIX  "
    write(fid,*) VMAX_NFIX
    write(fid,"(A)") "KM_NFIX    "
    write(fid,*) KM_NFIX
    write(fid,"(A)") "leafcn_obs       "
    write(fid,*) leafcn_obs
    write(fid,"(A)") "frootcn_obs      "
    write(fid,*) frootcn_obs
    write(fid,"(A)") "livewdcn_obs     "
    write(fid,*) livewdcn_obs
    write(fid,"(A)") "deadwdcn_obs     "
    write(fid,*) deadwdcn_obs
    write(fid,"(A)") "leafcp_obs       "
    write(fid,*) leafcp_obs
    write(fid,"(A)") "frootcp_obs      "
    write(fid,*) frootcp_obs
    write(fid,"(A)") "livewdcp_obs     "
    write(fid,*) livewdcp_obs
    write(fid,"(A)") "deadwdcp_obs     "
    write(fid,*) deadwdcp_obs
    write(fid,"(A)") "leafcn_obs_flex  "
    write(fid,*) leafcn_obs_flex
    write(fid,"(A)") "frootcn_obs_flex "
    write(fid,*) frootcn_obs_flex
    write(fid,"(A)") "livewdcn_obs_flex"
    write(fid,*) livewdcn_obs_flex
    write(fid,"(A)") "deadwdcn_obs_flex"
    write(fid,*) deadwdcn_obs_flex
    write(fid,"(A)") "leafcp_obs_flex  "
    write(fid,*) leafcp_obs_flex
    write(fid,"(A)") "frootcp_obs_flex "
    write(fid,*) frootcp_obs_flex
    write(fid,"(A)") "livewdcp_obs_flex"
    write(fid,*) livewdcp_obs_flex
    write(fid,"(A)") "deadwdcp_obs_flex"
    write(fid,*) deadwdcp_obs_flex
    write(fid,"(A)") "vcmax_np1        "
    write(fid,*) vcmax_np1
    write(fid,"(A)") "vcmax_np2        "
    write(fid,*) vcmax_np2
    write(fid,"(A)") "vcmax_np3        "
    write(fid,*) vcmax_np3
    write(fid,"(A)") "vcmax_np4        "
    write(fid,*) vcmax_np4
    write(fid,"(A)") "fnr    "
    write(fid,*) fnr
    write(fid,"(A)") "act25  "
    write(fid,*) act25
    write(fid,"(A)") "kcha   "
    write(fid,*) kcha
    write(fid,"(A)") "koha   "
    write(fid,*) koha
    write(fid,"(A)") "cpha   "
    write(fid,*) cpha
    write(fid,"(A)") "vcmaxha"
    write(fid,*) vcmaxha
    write(fid,"(A)") "jmaxha "
    write(fid,*) jmaxha
    write(fid,"(A)") "tpuha  "
    write(fid,*) tpuha
    write(fid,"(A)") "lmrha  "
    write(fid,*) lmrha
    write(fid,"(A)") "vcmaxhd"
    write(fid,*) vcmaxhd
    write(fid,"(A)") "jmaxhd "
    write(fid,*) jmaxhd
    write(fid,"(A)") "tpuhd  "
    write(fid,*) tpuhd
    write(fid,"(A)") "lmrhd  "
    write(fid,*) lmrhd
    write(fid,"(A)") "lmrse  "
    write(fid,*) lmrse
    write(fid,"(A)") "qe     "
    write(fid,*) qe
    write(fid,"(A)") "theta_cj"
    write(fid,*) theta_cj
    write(fid,"(A)") "bbbopt "
    write(fid,*) bbbopt
    write(fid,"(A)") "mbbopt "
    write(fid,*) mbbopt
    write(fid,"(A)") "nstor  "
    write(fid,*) nstor
    write(fid,"(A)") "br_xr  "
    write(fid,*) br_xr

    !-------- clm_varcon --------- !
    write(fid,"(A)") "zlak "
    write(fid,*) zlak
    write(fid,"(A)") "dzlak"
    write(fid,*) dzlak
    write(fid,"(A)") "zsoi "
    write(fid,*) zsoi
    write(fid,"(A)") "dzsoi"
    write(fid,*) dzsoi
    write(fid,"(A)") "zisoi"
    write(fid,*) zisoi
    write(fid,"(A)") "dzsoi_decomp"
    write(fid,*) dzsoi_decomp
    write(fid,"(A)") "nlvic  "
    write(fid,*) nlvic
    write(fid,"(A)") "dzvic  "
    write(fid,*) dzvic
    write(fid,"(A)") "zsoifl "
    write(fid,*) zsoifl
    write(fid,"(A)") "zisoifl"
    write(fid,*) zisoifl
    write(fid,"(A)") "dzsoifl"
    write(fid,*) dzsoifl

    !-------soilorder_varcon ------------------ !
    write(fid,"(A)") "smax            "
    write(fid,*) smax
    write(fid,"(A)") "ks_sorption     "
    write(fid,*) ks_sorption
    write(fid,"(A)") "r_weather       "
    write(fid,*) r_weather
    write(fid,"(A)") "r_adsorp        "
    write(fid,*) r_adsorp
    write(fid,"(A)") "r_desorp        "
    write(fid,*) r_desorp
    write(fid,"(A)") "r_occlude       "
    write(fid,*) r_occlude
    write(fid,"(A)") "k_s1_biochem    "
    write(fid,*) k_s1_biochem
    write(fid,"(A)") "k_s2_biochem    "
    write(fid,*) k_s2_biochem
    write(fid,"(A)") "k_s3_biochem    "
    write(fid,*) k_s3_biochem
    write(fid,"(A)") "k_s4_biochem    "
    write(fid,*) k_s4_biochem
    write(fid,"(A)") "r_mort_soilorder"
    write(fid,*) r_mort_soilorder

    !AllocParamsInst
    write(fid,"(A)") "AllocParamsInst%bdnr             "
    write(fid,*) AllocParamsInst%bdnr
    write(fid,"(A)") "AllocParamsInst%dayscrecover     "
    write(fid,*) AllocParamsInst%dayscrecover
    write(fid,"(A)") "AllocParamsInst%compet_plant_no3 "
    write(fid,*) AllocParamsInst%compet_plant_no3
    write(fid,"(A)") "AllocParamsInst%compet_plant_nh4 "
    write(fid,*) AllocParamsInst%compet_plant_nh4
    write(fid,"(A)") "AllocParamsInst%compet_decomp_no3"
    write(fid,*) AllocParamsInst%compet_decomp_no3
    write(fid,"(A)") "AllocParamsInst%compet_decomp_nh4"
    write(fid,*) AllocParamsInst%compet_decomp_nh4
    write(fid,"(A)") "AllocParamsInst%compet_denit     "
    write(fid,*) AllocParamsInst%compet_denit
    write(fid,"(A)") "AllocParamsInst%compet_nit       "
    write(fid,*) AllocParamsInst%compet_nit

    !PhenologyMod!
    write(fid,"(A)") "PhenolParamsInst%crit_dayl       "
    write(fid,*) PhenolParamsInst%crit_dayl
    write(fid,"(A)") "PhenolParamsInst%crit_dayl_stress"
    write(fid,*) PhenolParamsInst%crit_dayl_stress
    write(fid,"(A)") "PhenolParamsInst%cumprec_onset   "
    write(fid,*) PhenolParamsInst%cumprec_onset
    write(fid,"(A)") "PhenolParamsInst%ndays_on        "
    write(fid,*) PhenolParamsInst%ndays_on
    write(fid,"(A)") "PhenolParamsInst%ndays_off       "
    write(fid,*) PhenolParamsInst%ndays_off
    write(fid,"(A)") "PhenolParamsInst%fstor2tran      "
    write(fid,*) PhenolParamsInst%fstor2tran
    write(fid,"(A)") "PhenolParamsInst%crit_onset_fdd  "
    write(fid,*) PhenolParamsInst%crit_onset_fdd
    write(fid,"(A)") "PhenolParamsInst%crit_onset_swi  "
    write(fid,*) PhenolParamsInst%crit_onset_swi
    write(fid,"(A)") "PhenolParamsInst%soilpsi_on      "
    write(fid,*) PhenolParamsInst%soilpsi_on
    write(fid,"(A)") "PhenolParamsInst%crit_offset_fdd "
    write(fid,*) PhenolParamsInst%crit_offset_fdd
    write(fid,"(A)") "PhenolParamsInst%crit_offset_swi "
    write(fid,*) PhenolParamsInst%crit_offset_swi
    write(fid,"(A)") "PhenolParamsInst%soilpsi_off     "
    write(fid,*) PhenolParamsInst%soilpsi_off
    write(fid,"(A)") "PhenolParamsInst%lwtop           "
    write(fid,*) PhenolParamsInst%lwtop



    call fio_close(fid)

  end subroutine writeConstants

end module writeConstantsMod
