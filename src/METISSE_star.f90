subroutine METISSE_star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm,id)

    use track_support
    use interp_support
    use sse_support
    implicit none

    integer, intent(in), optional :: id
    real(dp) :: mass,mt,tm,tn,tscls(20),lums(10),GB(10),zpars(20)
    integer :: kw

    real(dp), allocatable:: rlist(:)
    real(dp) :: times_old(11),dtm, quant
    real(dp):: Mass_hold,delta1,delta_wind,delta,mnew

    integer :: idd, ierr, age_col,eep_m
    logical :: debug, exclude_core,consvR, mass_check
    type(track), pointer :: t

    idd = 1
    if(present(id)) idd = id
    t => tarr(idd)
            
    debug = .false.
!    if ((id == 1) .and. kw>=1)debug = .true.
!    if (t% star_type==rejuvenated) debug = .true.

    if (debug) print*, '-----------STAR---------------'
    if (debug) print*,"in star",mass,mt,kw,t% pars% phase,id,t% star_type
!    if (debug) print*,t% pars% age,dtm,t% pars% core_mass,t% pars% mass


    ierr = 0
    consvR = .false.
    delta = 0.d0
    t% zams_mass = mass
    eep_m = -1
    exclude_core = .false.

    if (t% pars% age<0.d0 .and. t% star_type/=unknown) then
        nullify(t); return
    endif

    !to be double sure, in case star changes kw/type outside hrdiag
    if(kw>= HeWD) then
        t% star_type = remnant
    elseif(kw>= He_MS .and. kw<=He_GB) then
        if (t% pars% phase< He_MS) t% star_type = switch
        if (use_sse_NHe) then
            t% star_type = sse_he_star
        else
            t% is_he_track = .true.
            age_col = i_he_age
        endif
    elseif(kw>= low_mass_MS) then
        t% is_he_track = .false.
        age_col = i_age
!        if (t% pars% phase>= He_MS .and. t% pars% phase<=He_GB) t% star_type = switch
        ! above line causes random segfaults if activated
    endif
        
    select case(t% star_type)
    case(unknown)
        ! Initial call- just do normal interpolation
        t% initial_mass = mass
        t% is_he_track = .false.

        call interpolate_mass(t,exclude_core)
        mt = t% tr(i_mass,ZAMS_EEP)
        t% pars% mass = mt
        t% initial_mass_old = t% initial_mass
        t% zams_mass_old = t% zams_mass
        if (debug)print*, "initial interpolate mass", t% initial_mass,t% zams_mass,t% pars% mass,mt,kw

        call calculate_timescales(t)
        !Todo: explain what age 2 and Times_new are
        t% times_new = t% times
        t% tr(i_age,:) = t% tr(i_age2,:)
        t% ms_old = t% times(MS)
        t% pars% age_old = t% pars% age
        t% pars% phase = kw
        !call write_eep_track(t,t% initial_mass)

    case(rejuvenated:switch) !gntage/mix/comenv

        if(debug.and.t% star_type==rejuvenated)print*,'rejuvenated star',t% pars% mass,mt
        if(debug.and.t% star_type==switch)print*, 'switching from', t% pars% phase,'to',kw

        if (t% pars% phase>=10 .and. kw<10) t% post_agb = .false.
        t% pars% mass = mt
        t% pars% delta = 0.d0
        t% pars% phase = kw
        t% initial_mass_old = t% initial_mass
        Mnew = t% pars% mass
        call get_initial_mass_for_new_track(t,idd,mnew,eep_m)
        call interpolate_mass(t,exclude_core)
!        t% zams_mass = t% initial_mass
        call calculate_timescales(t)
        t% times_new = t% times
        t% ms_old = t% MS_time
        t% pars% age_old = t% pars% age
        t% tr(age_col,:) = t% tr(i_age2,:)
        t% pars% dms = 0.d0
        if (eep_m>0 .and. eep_m<=t% ntrack) t% pars% age = min(t% tr(age_col,eep_m), t% times(11)-1d-6)
        
    case(sub_stellar:star_high_mass)
        if (debug) print*,'nuc burn star'
        
        ! Check if mass has changed since the last time star was called.
        IF(abs(mt-t% pars% mass)>1.0d-08 .and. (t% post_agb .eqv..false.) &
                .and. (t% pars% core_mass.lt.mt)) THEN
            if (debug) print*, 'diff in mt', mt, t% pars% mass,mt-t% pars% mass
            
            mass_check = .false.
            
            ! For tracks that already have wind mass loss, exclude contribution from winds
            if (t% has_mass_loss) then
                delta_wind = (t% pars% dms*dtm*1.0d+06)
            else
                delta_wind = 0.d0
            endif
            delta = (mt-t% pars% mass) + delta_wind
            
            if (debug)print*, 'delta org', delta,delta_wind,mt,t% pars% mass
            t% pars% delta = t% pars% delta+ delta
            delta1 = 2.0d-04*mt
            if (debug) print*, 'delta is', delta,t% pars% delta,delta1,t% pars% mass
            if (delta.ge.0.2*mt) then
                write(UNIT=err_unit,fmt=*)'large delta',delta,t% pars% mass,mt,kw,id
!               call stop_code
            endif
            if ((abs(t% pars% delta).ge.delta1)) then
                if(debug)print*,"interpolate mass called for",t% initial_mass,id

                t% ms_old = t% times(MS)
                t% pars% age_old = t% pars% age
                t% initial_mass_old = t% initial_mass
                Mnew = t% pars% mass+t% pars% delta
                call get_initial_mass_for_new_track(t,idd,mnew,eep_m)
                mass_check = .true.
                if (debug)print*, 'new initial mass',t% initial_mass
            endif
                
            IF(kw>MS .and. kw/=He_MS) THEN
                exclude_core = .true.
            else
                t% zams_mass_old = t% zams_mass
            ENDIF
        
            IF (dtm<0.d0) THEN
            ! print*,'dtm<0',t% pars% age,dtm
            ! check for phase or age reversals that may occur RLOF check
                if (t% pars% phase>= He_MS .and. kw<7)then
                    
                    if (debug)print*,'rev to old initial_mass for phase',t% pars% phase,kw
                    t% pars% phase = kw
                    if (abs(t% zams_mass_old-t% zams_mass)>1.0d-12) then
                        ! need new track core properties
                        if (debug) print*, 'diff in mass', t% zams_mass_old,t% zams_mass,t% zams_mass_old-t% zams_mass

                        eep_m = TAMS_EEP
                        if (t% is_he_track) eep_m = TAMS_HE_EEP
                        mass_hold = t% initial_mass
                        mnew = t% zams_mass_old
                        call get_initial_mass_for_new_track(t,idd,mnew,eep_m)
                        call interpolate_mass(t,exclude_core)
                        t% zams_mass_old = t% zams_mass
                        t% initial_mass = mass_hold
                        call calculate_timescales(t)
                    endif
                    t% initial_mass = t% initial_mass_old
                    mass_check = .true.
                elseif(exclude_core.eqv..false.) then
                    quant = (t% pars% age*t% MS_old/t% ms_time)+dtm
!                    if (id ==2)print*, 'quant', quant, t% pars% age_old
                    if(quant.le.t% pars% age_old) then
                        t% initial_mass = t% initial_mass_old
                        mass_check = .true.
                        if (debug)print*,'rev to old initial_mass',t% initial_mass,quant,t% pars% age_old
                    endif
                endif
            ENDIF
             
            if (mass_check) then
                ! reset delta
                t% pars% delta = 0.d0
                t% times_new = -1.d0
                
                if (exclude_core) then
                    !store core properties for post-main sequence evolution
                    if (debug) print*,'post-main-sequence star'
                    times_old = t% times

                    if ((t% pars% mcenv/t% pars% mass).ge.0.2d0) then
!                     .and.(t% pars% env_frac.ge.0.2)
                        allocate(rlist(t% ntrack))
                        rlist = t% tr(i_logR,:)
                        consvR= .true.
                    endif
    !
                    call interpolate_mass(t,exclude_core)
                   !write the mass interpolated track if write_eep_file is true
                    if (kw>=1 .and. kw<=4 .and. .false.) call write_eep_track(t,mt)
                    
                    call calculate_timescales(t)
                    t% times_new = t% times
                    t% times = times_old
                    t% nuc_time = t% times(11)
                    t% ms_time = t% times(MS)
                    if (t% is_he_track) t% ms_time = t% times(He_MS)

                    !TEST: R remains unchanged if Mcenv is significant
                    if (consvR) then
                        t% tr(i_logR,:) = rlist(1:t% ntrack)
                        deallocate(rlist)
                    endif
                else
                    ! kw=0,1,7: main-sequence star, rewrite all columns with new track
                    if (debug)print*, 'main-sequence star, rewrite with new track'
                    call interpolate_mass(t,exclude_core)
                    
                    call calculate_timescales(t)
                    t% times_new = t% times
                    t% tr(age_col,:) = t% tr(i_age2,:)
                endif
            endif
            t% pars% mass = mt
            ! above is to prevent multiple interpolations in mass
            ! for multiple calls to star in same step
        ENDIF
        
    case(remnant)
        tm = 1.0d+10
        tscls(1) = tm
        tn = 1.0d+10
        if (debug) print*, 'remnant'
    case(sse_he_star)
         call calculate_SSE_He_timescales(t)
         call calculate_SSE_He_star(t,tscls,lums,GB,tm,tn)
    end select
    
    if (kw<= TPAGB) then
        call calculate_SSE_parameters(t,zpars,tscls,lums,GB,tm,tn)
    elseif(kw>= He_MS .and. kw<=He_GB .and. (.not. use_sse_NHe)) then
        t% He_pars% Lzams = 10.d0**t% tr(i_logL, ZAMS_HE_EEP)
        t% He_pars% LtMS = 10.d0**t% tr(i_logL, TAMS_HE_EEP)
        t% He_pars% lx = 0.d0
        call calculate_SSE_He_star(t,tscls,lums,GB,tm,tn)
    endif
        
    t% MS_time = tm
    t% nuc_time = tn

    if (debug)print*, "in star end", mt,delta,kw,tm,tn,t%initial_mass,t% zams_mass
        if (debug) print*, '-----------------------------'

    nullify(t)
    return
end subroutine METISSE_star

