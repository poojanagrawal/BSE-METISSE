subroutine METISSE_zcnsts(z,zpars)
    use track_support
    use z_support
    use remnant_support

    real(dp), intent(in) :: z
    real(dp), intent(out) :: zpars(20)

    integer :: i,ierr,j,nmax
    logical :: debug
    
    ierr = 0
    debug = .false.
    
    if (initial_Z >0 .and.(relative_diff(initial_Z,z) < Z_accuracy_limit)) then
        if (debug) print*, '*****No change in metallicity, exiting METISSE_zcnsts.*****'
        return
    else
        if (debug) print*, '*****New metallicity is******',z,'initializing METISSE_zcnsts'
        if (allocated(s)) deallocate(s,key_cols,key_eeps)
        if (allocated(core_cols)) deallocate(core_cols)
        if (allocated(m_cutoff)) deallocate(m_cutoff)
        if (allocated(metallicity_file_list)) deallocate(metallicity_file_list)
        if (allocated(Mmax_array)) deallocate(Mmax_array, Mmin_array)

        i_mass = -1
        !TODO: re-initailize all such variables with defaults
    endif
    

    !reading defaults option first
    call read_defaults(ierr); if (ierr/=0) STOP
     verbose =.true.           
    if (front_end /= main) initial_Z = z

    !read inputs from evolve_metisse.in
    if (front_end == main .or. front_end == bse) then
        
        call read_metisse_input(ierr); if (ierr/=0) STOP
    elseif (front_end == COSMIC) then
        call get_metisse_input(TRACKS_DIR)
    else
        print*, "Error: Unrecongnized front_end_name for METISSE"
    endif
        
        
    !read metallicity related variables
    call get_metallcity_file_from_Z(initial_Z,ierr); if (ierr/=0) STOP
    
    
    if (front_end == COSMIC) then
        format_file = trim(TRACKS_DIR)//'/'//trim(format_file)
        INPUT_FILES_DIR = trim(TRACKS_DIR)//'/'//trim(INPUT_FILES_DIR)
    endif
    
    !read file-format
    call read_format(format_file,ierr); if (ierr/=0) STOP

    !get filenames
    if (trim(INPUT_FILES_DIR) == '' )then
        print*,"Error: INPUT_FILES_DIR is not defined"
        STOP
    endif
        
    if (verbose) print*,"Reading input files from: ", trim(INPUT_FILES_DIR)
    
    call get_files_from_path(INPUT_FILES_DIR,file_extension,track_list,ierr)
    
    if (ierr/=0) then
        print*,'Error: failed to read input files.'
        print*,'Check if INPUT_FILES_DIR is correct.'
        STOP
    endif

    num_tracks = size(track_list)
    allocate(s(num_tracks))
    s% filename = track_list
    if (verbose) print*,"Number of input tracks: ", num_tracks

    call read_key_eeps()
    if (debug) print*, "key eeps", key_eeps

    if (read_eep_files) then
        if (debug) print*,"reading eep files"
        do i=1,num_tracks
            call read_eep(s(i))
            if(debug) write(*,'(a50,f8.2,99i8)') trim(s(i)% filename), s(i)% initial_mass, s(i)% ncol
        end do
    else
        !read and store column names in temp_cols from the the file if header location is not provided
        if (header_location<=0) then
            if (debug) print*,"Reading column names from file"

            call process_columns(column_name_file,temp_cols,ierr)
            
            if(ierr/=0) then
                print*,"Failed while trying to read column_name_file"
                print*,"Check if header location and column_name_file are correct "
                STOP
            endif

            if (size(temp_cols) /= total_cols) then
                print*,'Number of columns in the column_name_file does not matches with the total_cols'
                print*,'Check if column_name_file and total_cols are correct'
                STOP
            endif
        end if

        do i=1,num_tracks
            call read_input_file(s(i))
            if(debug) write(*,'(a50,f8.2,99i8)') trim(s(i)% filename), s(i)% initial_mass, s(i)% ncol
        end do

    endif

!    if(debug) print*, s(1)% cols% name, s(1)% tr(:,1)
    
    nmax = maxval(s% ntrack)
    allocate(Mmax_array(nmax), Mmin_array(nmax))
    Mmax_array = 0.d0
    Mmin_array = 1000000000 !(random big number)
    
    do i = 1,size(s)
        s(i)% has_mass_loss = check_mass_loss(s(i))
        
        do j = 1, nmax
            if (s(i)% ntrack>=j) then
                Mmax_array(j) = max(Mmax_array(j),s(i)% tr(i_mass,j))
                Mmin_array(j) = min(Mmin_array(j),s(i)% tr(i_mass,j))
            endif
        end do
    end do

    !TODO: check for monotonicity of initial masses
    if (debug) print*,s% initial_mass

    !first calculate it the SSE way for use as backup 
    call calculate_sse_zpars(z,zpars)

    !then reset z parameters where available
    !and determine cutoff masses
    call set_zparameters(zpars)

    if (front_end == main) then
    ! sets remnant schmeme from SSE_input_controls
        call assign_commons_main()
    else
    ! reads
        call assign_commons()
    endif

end subroutine METISSE_zcnsts

