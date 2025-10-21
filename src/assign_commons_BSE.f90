subroutine assign_commons()
    use track_support
    use remnant_support, only:ns_flag,wd_flag,if_flag,Max_NS_mass,ec_flag
    implicit none
    
    !to assign common variables when METISSE is used with BSE
      
    INTEGER :: ceflag,tflag,ifflag,nsflag,wdflag
    COMMON /FLAGS/ ceflag,tflag,ifflag,nsflag,wdflag

    REAL(dp) :: neta,bwind,hewind,mxns
    COMMON /VALUE1/ neta,bwind,hewind,mxns
        
    if(front_end == BSE) then
    ! use inputs from BSE/SSE
        ns_flag = nsflag
        wd_flag = wdflag
        if_flag = ifflag
        Max_NS_mass = mxns
        ec_flag = 1
!        print*, 'bse assign commons', ns_flag,nsflag,Max_NS_mass
        
    !neta,bwind,hewind,sigma
    !pts1,pt2,pts3
    else
        print*,'Error: Front end mismtach in assign commons'
        print*,'expected 1 (BSE/SSE); got ', front_end
    endif

end subroutine

logical function check_path_change() result (load_tracks)
    load_tracks = .true.
end function
