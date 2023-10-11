real(dp) function metisse_mlwind(kw,lum,r,mt,mc,rl,z,id)
    use track_support
    use interp_support, only: interpolate_age
    implicit none
    integer, intent(in), optional :: id
    
    integer:: kw,idd

    real(dp) :: lum,r,mt,mc,rl,z
    real(dp) :: dms
    real(dp) :: tnext,tprev, mnext,mprev

    logical :: add_mass_loss
    real(dp) :: SSE_mlwind
    external SSE_mlwind
    logical :: debug
    type(track), pointer :: t

    idd = 1

    if(present(id)) idd = id
    t => tarr(idd)
        
    debug = .false.
!    if (id==1) debug = .true.
    if (debug) print*, 'in MLWIND', id,idd,mt,t% pars% mass
    ! if tracks don't have mass loss already, use SSE's wind routine
    ! TODO: move it to input file
    add_mass_loss = .true.

    dms = 0.d0
    if (t% has_mass_loss .and. kw<6) then
        tnext = t% pars% age+ t% pars% dt
        tprev = max(0.d0,t% pars% age-t% pars% dt)
!        print*,'in mlwind',t% pars% age,t% pars% dt,tprev,tnext
        if (tprev<=0.d0) then
            !Forward finite difference
            if (debug) print*, 'calling interpolate age for ini', tnext
            call interpolate_age(t, tnext, i_mass, mnext)
            if (t% pars% dt>0.d0) dms = abs(mt-mnext)/(t% pars% dt*1E+6)

        elseif (tnext>= t% nuc_time) then
            if (debug) print*, 'calling interpolate age for fin', tprev
            call interpolate_age(t, tprev, i_mass, mprev)
            if (t% pars% dt>0.d0) dms = abs(mprev-mt)/(t% pars% dt*1E+6)
        else
!            tnext = min(t% nuc_time, tnext)
            if (debug) print*, 'calling interpolate age for ', tnext
            call interpolate_age(t, tnext, i_mass, mnext)
!            tprev = max(0.d0,tprev)
            if (debug) print*, 'calling interpolate age for ', tprev
            call interpolate_age(t, tprev, i_mass, mprev)
            if (t% pars% dt>0.d0) dms = abs(mprev-mnext)/(2*t% pars% dt*1E+6)
        endif

        t% pars% dms = dms

        !Using 'abs' as sometime dms is negative due to rounding errors
        if (debug) print*, 'track has mass loss',t% initial_mass,dms,kw,mt,t% pars% mass
    else
        if (add_mass_loss) dms = SSE_mlwind(kw,lum,r,mt,mc,rl,z)
!        if (kw<=9) print*,"mlwind function",dms,mt,mc,kw,id
    endif
    if (kw ==6 .and. t% post_agb) dms = 0.d0
    if (debug) print*,"in metisse_mlwind, dms",dms, t% pars% mass, t% pars% phase
    metisse_mlwind = dms
    
    nullify(t)
end function

