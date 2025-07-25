module FrictionVelocityMod

#include "shr_assert.h"

  !------------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculation of the friction velocity, relation for potential
  ! temperature and humidity profiles of surface boundary layer.
  !
  ! !USES:
  use shr_kind_mod         , only : r8 => shr_kind_r8
  use shr_log_mod          , only : errMsg => shr_log_errMsg
  use FrictionVelocityType , only : frictionvel_type
  use LandunitType         , only : lun_pp
  use VegetationType       , only : veg_pp
  !
  ! !PUBLIC TYPES:
  implicit none
  save

  logical, public :: implicit_stress = .false.
  logical, public :: atm_gustiness = .false.
  logical, public :: force_land_gustiness = .false.
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: FrictionVelocity       ! Calculate friction velocity
  public :: MoninObukIni           ! Initialization of the Obukhov length scale
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: StabilityFunc1        ! Stability function for rib < 0.
  private :: StabilityFunc2        ! Stability function for rib < 0.
  !------------------------------------------------------------------------------


contains

  !------------------------------------------------------------------------------
  subroutine FrictionVelocity(lbn, ubn, fn, filtern, &
       displa, z0m, z0h, z0q, &
       obu, iter, ur, um, ugust, ustar, &
       temp1, temp2, temp12m, temp22m, fm,frictionvel_vars,landunit_index)
    !$acc routine seq
    ! !DESCRIPTION:
    ! Calculation of the friction velocity, relation for potential
    ! temperature and humidity profiles of surface boundary layer.
    ! The scheme is based on the work of Zeng et al. (1998):
    ! Intercomparison of bulk aerodynamic algorithms for the computation
    ! of sea surface fluxes using TOGA CORE and TAO data. J. Climate,
    ! Vol. 11, 2628-2644.
    !
    ! !USES:
    use elm_varcon, only : vkc


    implicit none
    !
    ! !ARGUMENTS:
    integer  , intent(in)    :: lbn, ubn                 ! pft/landunit array bounds
    integer  , intent(in)    :: fn                       ! number of filtered pft/landunit elements
    integer  , intent(in)    :: filtern(fn)              ! pft/landunit filter
    real(r8) , intent(in)    :: displa  ( lbn: )         ! displacement height (m) [lbn:ubn]
    real(r8) , intent(in)    :: z0m     ( lbn: )         ! roughness length over vegetation, momentum [m] [lbn:ubn]
    real(r8) , intent(in)    :: z0h     ( lbn: )         ! roughness length over vegetation, sensible heat [m] [lbn:ubn]
    real(r8) , intent(in)    :: z0q     ( lbn: )         ! roughness length over vegetation, latent heat [m] [lbn:ubn]
    real(r8) , intent(in)    :: obu     ( lbn: )         ! Obukhov length scale (m) [lbn:ubn]
    integer  , intent(in)    :: iter                     ! iteration number
    real(r8) , intent(in)    :: ur      ( lbn: )         ! wind speed at reference height [m/s] [lbn:ubn]
    real(r8) , intent(in)    :: um      ( lbn: )         ! wind speed including the stablity effect [m/s] [lbn:ubn]
    real(r8) , intent(in)    :: ugust   ( lbn: )         ! Gustiness wind speed [m/s] [lbn:ubn]
    real(r8) , intent(out)   :: ustar   ( lbn: )         ! friction velocity [m/s] [lbn:ubn]
    real(r8) , intent(out)   :: temp1   ( lbn: )         ! relation for potential temperature profile [lbn:ubn]
    real(r8) , intent(out)   :: temp12m ( lbn: )         ! relation for potential temperature profile applied at 2-m [lbn:ubn]
    real(r8) , intent(out)   :: temp2   ( lbn: )         ! relation for specific humidity profile [lbn:ubn]
    real(r8) , intent(out)   :: temp22m ( lbn: )         ! relation for specific humidity profile applied at 2-m [lbn:ubn]
    real(r8) , intent(inout) :: fm      ( lbn: )         ! diagnose 10m wind (DUST only) [lbn:ubn]
    type(frictionvel_type) , intent(inout) :: frictionvel_vars
    logical  , intent(in), optional :: landunit_index   ! optional argument that defines landunit or pft level
    !
    ! !LOCAL VARIABLES:
    real(r8), parameter :: zetam = 1.574_r8 ! transition point of flux-gradient relation (wind profile)
    real(r8), parameter :: zetat = 0.465_r8 ! transition point of flux-gradient relation (temp. profile)
    integer  :: f                           ! pft/landunit filter index
    integer  :: n                           ! pft/landunit index
    integer  :: g                           ! gridcell index
    integer  :: pp                          ! pfti,pftf index
    integer :: pfti, pftf
    real(r8) :: zldis              ! reference height "minus" zero displacement heght [m]
    real(r8) :: zeta               ! dimensionless height used in Monin-Obukhov theory
    real(r8) :: tmp1,tmp2,tmp3,tmp4         ! Used to diagnose the 10 meter wind
    real(r8) :: fmnew                       ! Used to diagnose the 10 meter wind
    real(r8) :: fm10                        ! Used to diagnose the 10 meter wind
    real(r8) :: zeta10                      ! Used to diagnose the 10 meter wind
    real(r8) :: vds_tmp                     ! Temporary for dry deposition velocity
    !------------------------------------------------------------------------------
    associate(                                                   &
         forc_hgt_u_patch => frictionvel_vars%forc_hgt_u_patch , & ! Input:  [real(r8) (:) ] observational height of wind at pft level [m]
         forc_hgt_t_patch => frictionvel_vars%forc_hgt_t_patch , & ! Input:  [real(r8) (:) ] observational height of temperature at pft level [m]
         forc_hgt_q_patch => frictionvel_vars%forc_hgt_q_patch , & ! Input:  [real(r8) (:) ] observational height of specific humidity at pft level [m]
         vds              => frictionvel_vars%vds_patch        , & ! Output: [real(r8) (:) ] dry deposition velocity term (m/s) (for SO4 NH4NO3)
         u10              => frictionvel_vars%u10_patch        , & ! Output: [real(r8) (:) ] 10-m wind (m/s) (for dust model)
         u10_elm          => frictionvel_vars%u10_elm_patch    , & ! Output: [real(r8) (:) ] 10-m wind (m/s)
         u10_with_gusts_elm=>frictionvel_vars%u10_with_gusts_elm_patch, & ! Output: [real(r8) (:) ] 10-m wind with gusts(m/s)
         va               => frictionvel_vars%va_patch         , & ! Output: [real(r8) (:) ] atmospheric wind speed plus convective velocity (m/s)
         fv               => frictionvel_vars%fv_patch           & ! Output: [real(r8) (:) ] friction velocity (m/s) (for dust model)
         )

      ! Adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

        do f = 1, fn
          n = filtern(f)

          if (present(landunit_index)) then
             g = lun_pp%gridcell(n)
             pfti = lun_pp%pfti(n)
             pftf = lun_pp%pftf(n)
          else
             g = veg_pp%gridcell(n)
          end if

          ! Wind profile

          if (present(landunit_index)) then
             zldis = forc_hgt_u_patch(pfti)-displa(n)
          else
             zldis = forc_hgt_u_patch(n)-displa(n)
          end if

          zeta = zldis/obu(n)
          if (zeta < -zetam) then
             ustar(n) = vkc*um(n)/(log(-zetam*obu(n)/z0m(n))&
                  - StabilityFunc1(-zetam) &
                  + StabilityFunc1(z0m(n)/obu(n)) &
                  + 1.14_r8*((-zeta)**0.333_r8-(zetam)**0.333_r8))
          else if (zeta < 0._r8) then
             ustar(n) = vkc*um(n)/(log(zldis/z0m(n))&
                  - StabilityFunc1(zeta)&
                  + StabilityFunc1(z0m(n)/obu(n)))
          else if (zeta <=  1._r8) then
             ustar(n) = vkc*um(n)/(log(zldis/z0m(n)) + 5._r8*zeta -5._r8*z0m(n)/obu(n))
          else
             ustar(n) = vkc*um(n)/(log(obu(n)/z0m(n))+5._r8-5._r8*z0m(n)/obu(n) &
                  +(5._r8*log(zeta)+zeta-1._r8))
          end if

          if (zeta < 0._r8) then
             vds_tmp = 2.e-3_r8*ustar(n) * ( 1._r8 + (300._r8/(-obu(n)))**0.666_r8)
          else
             vds_tmp = 2.e-3_r8*ustar(n)
          endif

          if (present(landunit_index)) then
             do pp = pfti,pftf
                vds(pp) = vds_tmp
             end do
          else
             vds(n) = vds_tmp
          end if

          ! Calculate a 10-m wind (10m + z0m + d)
          ! For now, this will not be the same as the 10-m wind calculated for the dust
          ! model because the CLM stability functions are used here, not the LSM stability
          ! functions used in the dust model. We will eventually change the dust model to be
          ! consistent with the following formulation.
          ! Note that the 10-m wind calculated this way could actually be larger than the
          ! atmospheric forcing wind because 1) this includes the convective velocity, 2)
          ! this includes the 1 m/s minimum wind threshold

          ! If forcing height is less than or equal to 10m, then set 10-m wind to um
          if (present(landunit_index)) then
             do pp = pfti,pftf
                if (zldis-z0m(n) <= 10._r8) then
                   u10_with_gusts_elm(pp) = um(n)
                else
                   if (zeta < -zetam) then
                      u10_with_gusts_elm(pp) = um(n) - ( ustar(n)/vkc*(log(-zetam*obu(n)/(10._r8+z0m(n))) &
                           - StabilityFunc1(-zetam)                              &
                           + StabilityFunc1((10._r8+z0m(n))/obu(n))              &
                           + 1.14_r8*((-zeta)**0.333_r8-(zetam)**0.333_r8)) )
                   else if (zeta < 0._r8) then
                      u10_with_gusts_elm(pp) = um(n) - ( ustar(n)/vkc*(log(zldis/(10._r8+z0m(n))) &
                           - StabilityFunc1(zeta)                             &
                           + StabilityFunc1((10._r8+z0m(n))/obu(n))) )
                   else if (zeta <=  1._r8) then
                      u10_with_gusts_elm(pp) = um(n) - ( ustar(n)/vkc*(log(zldis/(10._r8+z0m(n))) &
                           + 5._r8*zeta - 5._r8*(10._r8+z0m(n))/obu(n)) )
                   else
                      u10_with_gusts_elm(pp) = um(n) - ( ustar(n)/vkc*(log(obu(n)/(10._r8+z0m(n))) &
                           + 5._r8 - 5._r8*(10._r8+z0m(n))/obu(n)                &
                           + (5._r8*log(zeta)+zeta-1._r8)) )

                   end if
                end if
                va(pp) = um(n)
                ! Estimate u10 with effects of gustiness removed.
                u10_elm(pp) = u10_with_gusts_elm(pp) * sqrt(max(0., um(n)**2 - ugust(n)**2)) / um(n)
             end do
          else
             if (zldis-z0m(n) <= 10._r8) then
                u10_with_gusts_elm(n) = um(n)
             else
                if (zeta < -zetam) then
                   u10_with_gusts_elm(n) = um(n) - ( ustar(n)/vkc*(log(-zetam*obu(n)/(10._r8+z0m(n))) &
                        - StabilityFunc1(-zetam)                                 &
                        + StabilityFunc1((10._r8+z0m(n))/obu(n))                 &
                        + 1.14_r8*((-zeta)**0.333_r8-(zetam)**0.333_r8)) )
                else if (zeta < 0._r8) then
                   u10_with_gusts_elm(n) = um(n) - ( ustar(n)/vkc*(log(zldis/(10._r8+z0m(n))) &
                        - StabilityFunc1(zeta)                                &
                        + StabilityFunc1((10._r8+z0m(n))/obu(n))) )
                else if (zeta <=  1._r8) then
                   u10_with_gusts_elm(n) = um(n) - ( ustar(n)/vkc*(log(zldis/(10._r8+z0m(n))) &
                        + 5._r8*zeta - 5._r8*(10._r8+z0m(n))/obu(n)) )
                else
                   u10_with_gusts_elm(n) = um(n) - ( ustar(n)/vkc*(log(obu(n)/(10._r8+z0m(n))) &
                        + 5._r8 - 5._r8*(10._r8+z0m(n))/obu(n)                   &
                        + (5._r8*log(zeta)+zeta-1._r8)) )
                end if
             end if
             va(n) = um(n)
             ! Estimate u10 with effects of gustiness removed.
             u10_elm(n) = u10_with_gusts_elm(n) * sqrt(max(0., um(n)**2 - ugust(n)**2)) / um(n)
          end if

          !===================!
          !Temperature Profile!
          !===================!
          if (present(landunit_index)) then
             zldis = forc_hgt_t_patch(pfti)-displa(n)
          else
             zldis = forc_hgt_t_patch(n)-displa(n)
          end if
          zeta = zldis/obu(n)
          if (zeta < -zetat) then
             temp1(n) = vkc/(log(-zetat*obu(n)/z0h(n))&
                  - StabilityFunc2(-zetat) &
                  + StabilityFunc2(z0h(n)/obu(n)) &
                  + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta)**(-0.333_r8)))
          else if (zeta < 0._r8) then
             temp1(n) = vkc/(log(zldis/z0h(n)) &
                  - StabilityFunc2(zeta) &
                  + StabilityFunc2(z0h(n)/obu(n)))
          else if (zeta <=  1._r8) then
             temp1(n) = vkc/(log(zldis/z0h(n)) + 5._r8*zeta - 5._r8*z0h(n)/obu(n))
          else
             temp1(n) = vkc/(log(obu(n)/z0h(n)) + 5._r8 - 5._r8*z0h(n)/obu(n) &
                  + (5._r8*log(zeta)+zeta-1._r8))
          end if
          !=================!
          !Humidity Profile !
          !=================!
          if (present(landunit_index)) then
             if (forc_hgt_q_patch(pfti) == forc_hgt_t_patch(pfti) .and. z0q(n) == z0h(n)) then
                temp2(n) = temp1(n)
             else
                zldis = forc_hgt_q_patch(pfti)-displa(n)
                zeta = zldis/obu(n)
                if (zeta < -zetat) then
                   temp2(n) = vkc/(log(-zetat*obu(n)/z0q(n)) &
                        - StabilityFunc2(-zetat) &
                        + StabilityFunc2(z0q(n)/obu(n)) &
                        + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta)**(-0.333_r8)))
                else if (zeta < 0._r8) then
                   temp2(n) = vkc/(log(zldis/z0q(n)) &
                        - StabilityFunc2(zeta) &
                        + StabilityFunc2(z0q(n)/obu(n)))
                else if (zeta <=  1._r8) then
                   temp2(n) = vkc/(log(zldis/z0q(n)) + 5._r8*zeta-5._r8*z0q(n)/obu(n))
                else
                   temp2(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                        + (5._r8*log(zeta)+zeta-1._r8))
                end if
             end if
          else
             if (forc_hgt_q_patch(n) == forc_hgt_t_patch(n) .and. z0q(n) == z0h(n)) then
                temp2(n) = temp1(n)
             else
                zldis = forc_hgt_q_patch(n)-displa(n)
                zeta = zldis/obu(n)
                if (zeta < -zetat) then
                   temp2(n) = vkc/(log(-zetat*obu(n)/z0q(n)) &
                        - StabilityFunc2(-zetat) &
                        + StabilityFunc2(z0q(n)/obu(n)) &
                        + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta)**(-0.333_r8)))
                else if (zeta < 0._r8) then
                   temp2(n) = vkc/(log(zldis/z0q(n)) &
                        - StabilityFunc2(zeta) &
                        + StabilityFunc2(z0q(n)/obu(n)))
                else if (zeta <=  1._r8) then
                   temp2(n) = vkc/(log(zldis/z0q(n)) + 5._r8*zeta-5._r8*z0q(n)/obu(n))
                else
                   temp2(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                        + (5._r8*log(zeta)+zeta-1._r8))
                end if
             endif
          endif

          ! Temperature profile applied at 2-m

          zldis = 2.0_r8 + z0h(n)
          zeta = zldis/obu(n)
          if (zeta < -zetat) then
             temp12m(n) = vkc/(log(-zetat*obu(n)/z0h(n))&
                  - StabilityFunc2(-zetat) &
                  + StabilityFunc2(z0h(n)/obu(n)) &
                  + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta)**(-0.333_r8)))
          else if (zeta < 0._r8) then
             temp12m(n) = vkc/(log(zldis/z0h(n)) &
                  - StabilityFunc2(zeta)  &
                  + StabilityFunc2(z0h(n)/obu(n)))
          else if (zeta <=  1._r8) then
             temp12m(n) = vkc/(log(zldis/z0h(n)) + 5._r8*zeta - 5._r8*z0h(n)/obu(n))
          else
             temp12m(n) = vkc/(log(obu(n)/z0h(n)) + 5._r8 - 5._r8*z0h(n)/obu(n) &
                  + (5._r8*log(zeta)+zeta-1._r8))
          end if

          ! Humidity profile applied at 2-m

          if (z0q(n) == z0h(n)) then
             temp22m(n) = temp12m(n)
          else
             zldis = 2.0_r8 + z0q(n)
             zeta = zldis/obu(n)
             if (zeta < -zetat) then
                temp22m(n) = vkc/(log(-zetat*obu(n)/z0q(n)) - &
                     StabilityFunc2(-zetat) + StabilityFunc2(z0q(n)/obu(n)) &
                     + 0.8_r8*((zetat)**(-0.333_r8)-(-zeta)**(-0.333_r8)))
             else if (zeta < 0._r8) then
                temp22m(n) = vkc/(log(zldis/z0q(n)) - &
                     StabilityFunc2(zeta)+StabilityFunc2(z0q(n)/obu(n)))
             else if (zeta <=  1._r8) then
                temp22m(n) = vkc/(log(zldis/z0q(n)) + 5._r8*zeta-5._r8*z0q(n)/obu(n))
             else
                temp22m(n) = vkc/(log(obu(n)/z0q(n)) + 5._r8 - 5._r8*z0q(n)/obu(n) &
                     + (5._r8*log(zeta)+zeta-1._r8))
             end if
          end if

          ! diagnose 10-m wind for dust model (dstmbl.F)
          ! Notes from C. Zender's dst.F:
          ! According to Bon96 p. 62, the displacement height d (here displa) is
          ! 0.0 <= d <= 0.34 m in dust source regions (i.e., regions w/o trees).
          ! Therefore d <= 0.034*z1 and may safely be neglected.
          ! Code from LSM routine SurfaceTemperature was used to obtain u10


          if (present(landunit_index)) then
             zldis = forc_hgt_u_patch(pfti)-displa(n)
          else
             zldis = forc_hgt_u_patch(n)-displa(n)
          end if

          zeta = zldis/obu(n)
          if (min(zeta, 1._r8) < 0._r8) then
             tmp1 = (1._r8 - 16._r8*min(zeta,1._r8))**0.25_r8
             tmp2 = log((1._r8+tmp1*tmp1)/2._r8)
             tmp3 = log((1._r8+tmp1)/2._r8)
             fmnew = 2._r8*tmp3 + tmp2 - 2._r8*atan(tmp1) + 1.5707963_r8
          else
             fmnew = -5._r8*min(zeta,1._r8)
          endif
          if (iter == 1) then
              fm(n) = fmnew
          else
              fm(n) = 0.5_r8 * (fm(n)+fmnew)
          end if
          zeta10 = min(10._r8/obu(n), 1._r8)
          if (zeta == 0._r8) zeta10 = 0._r8
          if (zeta10 < 0._r8) then
             tmp1 = (1.0_r8 - 16.0_r8 * zeta10)**0.25_r8
             tmp2 = log((1.0_r8 + tmp1*tmp1)/2.0_r8)
             tmp3 = log((1.0_r8 + tmp1)/2.0_r8)
             fm10 = 2.0_r8*tmp3 + tmp2 - 2.0_r8*atan(tmp1) + 1.5707963_r8
          else                ! not stable
             fm10 = -5.0_r8 * zeta10
          end if

          if (present(landunit_index)) then
             tmp4 = log( max( 1.0_8, forc_hgt_u_patch(pfti) / 10._r8) )
          else
             tmp4 = log( max( 1.0_8, forc_hgt_u_patch(n) / 10._r8) )
          end if
          if (present(landunit_index)) then
             do pp = pfti,pftf
                u10(pp) = ur(n) - ustar(n)/vkc * (tmp4 - fm(n) + fm10)
                fv(pp)  = ustar(n)
             end do
          else
             u10(n) = ur(n) - ustar(n)/vkc * (tmp4 - fm(n) + fm10)
             fv(n)  = ustar(n)
          end if
        end do !! do loop of fn

     end associate

  end subroutine FrictionVelocity

  !------------------------------------------------------------------------------
  real(r8) function StabilityFunc1(zeta)
    !$acc routine seq
    ! !DESCRIPTION:
    ! Stability function for rib < 0.
    !
    ! !USES:
    use shr_const_mod, only: SHR_CONST_PI
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(in) :: zeta  ! dimensionless height used in Monin-Obukhov theory
    !
    ! !LOCAL VARIABLES:
    real(r8) :: chik, chik2
    !------------------------------------------------------------------------------

    chik2 = sqrt(1._r8-16._r8*zeta)
    chik = sqrt(chik2)
    StabilityFunc1 = 2._r8*log((1._r8+chik)*0.5_r8) &
         + log((1._r8+chik2)*0.5_r8)-2._r8*atan(chik)+SHR_CONST_PI*0.5_r8

  end function StabilityFunc1

  !------------------------------------------------------------------------------
  real(r8) function StabilityFunc2(zeta)
    !$acc routine seq
    ! !DESCRIPTION:
    ! Stability function for rib < 0.
    !
    ! !USES:
    use shr_const_mod, only: SHR_CONST_PI
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(in) :: zeta  ! dimensionless height used in Monin-Obukhov theory
    !
    ! !LOCAL VARIABLES:
    real(r8) :: chik2
    !------------------------------------------------------------------------------

    chik2 = sqrt(1._r8-16._r8*zeta)
    StabilityFunc2 = 2._r8*log((1._r8+chik2)*0.5_r8)

  end function StabilityFunc2

  !-----------------------------------------------------------------------
  subroutine MoninObukIni (ur, thv, dthv, zldis, z0m, um, obu)
    !$acc routine seq
    ! !DESCRIPTION:
    ! Initialization of the Obukhov length scale.
    ! The scheme is based on the work of Zeng et al. (1998):
    ! Intercomparison of bulk aerodynamic algorithms for the computation
    ! of sea surface fluxes using TOGA CORE and TAO data. J. Climate,
    ! Vol. 11, 2628-2644.
    !
    ! !USES:
    use elm_varcon, only : grav
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(in)  :: ur    ! wind speed at reference height [m/s]
    real(r8), intent(in)  :: thv   ! virtual potential temperature (kelvin)
    real(r8), intent(in)  :: dthv  ! diff of vir. poten. temp. between ref. height and surface
    real(r8), intent(in)  :: zldis ! reference height "minus" zero displacement heght [m]
    real(r8), intent(in)  :: z0m   ! roughness length, momentum [m]
    real(r8), intent(out) :: um    ! wind speed including the stability effect [m/s]
    real(r8), intent(out) :: obu   ! Obukhov length scale (m)
    !
    ! !LOCAL VARIABLES:
    real(r8) :: wc    ! convective velocity [m/s]
    real(r8) :: rib   ! bulk Richardson number
    real(r8) :: zeta  ! dimensionless height used in Monin-Obukhov theory
    real(r8) :: ustar ! friction velocity [m/s]
    !-----------------------------------------------------------------------

    ! Initial values of u* and convective velocity

    ustar=0.06_r8
    wc=0.5_r8
    if (dthv >= 0._r8) then
       um=max(ur,0.1_r8)
    else
       um=sqrt(ur*ur+wc*wc)
    endif

    rib=grav*zldis*dthv/(thv*um*um)

    if (rib >= 0._r8) then      ! neutral or stable
       zeta = rib*log(zldis/z0m)/(1._r8-5._r8*min(rib,0.19_r8))
       zeta = min(2._r8,max(zeta,0.01_r8 ))
    else                     ! unstable
       zeta=rib*log(zldis/z0m)
       zeta = max(-100._r8,min(zeta,-0.01_r8 ))
    endif

    obu=zldis/zeta

  end subroutine MoninObukIni

end module FrictionVelocityMod
