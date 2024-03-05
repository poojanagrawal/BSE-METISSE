subroutine METISSE_zcnsts(z,zpars)
    use track_support
    use z_support
    use remnant_support

    real(dp), intent(in) :: z
    real(dp), intent(out) :: zpars(20)

    integer :: i,ierr,j,nloop,jerr
    logical :: debug, res
    logical :: read_inputs = .true.
    character(LEN=strlen), allocatable :: track_list(:)
    character(LEN=strlen) :: USE_DIR, find_cmd,rnd

    debug = .false.
    
    if (initial_Z >0 .and.(relative_diff(initial_Z,z) < Z_accuracy_limit) .and. zpars(14).gt.0.d0) then
        if (debug) print*, '**** No change in metallicity, exiting METISSE_zcnsts ****'
        return
    endif
    
    if (debug) print*, '**** Metallicity is ',z,'initializing METISSE_zcnsts ****'
    
    ierr = 0
    nloop = 2
    use_sse_NHe = .true.
    
    if (allocated(core_cols)) deallocate(core_cols)
    if (allocated(m_cutoff)) deallocate(m_cutoff)
    if (allocated(Mmax_array)) deallocate(Mmax_array, Mmin_array)
    
    if (read_inputs) then
        
        !reading defaults option first
        call read_defaults(ierr); if (ierr/=0) STOP

        !read user inputs from evolve_metisse.in or use inputs from code directly
        if (front_end == main .or. front_end == bse) then
            call read_metisse_input(ierr); if (ierr/=0) STOP
        elseif (front_end == COSMIC) then
            call get_metisse_input(TRACKS_DIR,metallicity_file_list)
            if (TRACKS_DIR_HE/='') call get_metisse_input(TRACKS_DIR_HE,metallicity_file_list_he)
        else
            print*, "Error: reading inputs; unrecognized front_end_name for METISSE"
        endif
        
        metallicity_file_list = pack(metallicity_file_list,mask=len_trim(metallicity_file_list)>0)
        
        metallicity_file_list_he = pack(metallicity_file_list_he,mask=len_trim(metallicity_file_list_he)>0)
        
        if (size(metallicity_file_list)<1) then
            print*, "Error: metallicity_file_list is empty"
            STOP
        endif
        
        if (size(metallicity_file_list_he)<1) then
            print*, "Error: metallicity_file_list_he is empty"
            print*, "Switching to SSE formulae for helium stars "
            nloop = 1
        endif
        
        if(debug) print*,'metallicity files: ',metallicity_file_list
        if(debug) print*,'metallicity files he : ', metallicity_file_list_he
        
        !Some unit numbers are reserved: 5 is standard input, 6 is standard output.
        if (write_error_to_file) then
            err_unit = 99   !will write to fort.99
        else
            err_unit = 6      !will write to screen
        endif
    
        read_inputs = .false.
    end if
    
    if (front_end /= main) initial_Z = z
    
    !first calculate zpars the SSE way for use as backup
    call calculate_sse_zpars(z,zpars)
    
    ! need to intialize these seperately as they may be
    ! used uninitialized if he tracks are not present
    i_he_RCO = -1
    i_he_mcenv = -1
    i_he_Rcenv = -1
    i_he_MoI = -1
    i_he_age = -1
            
    do i = nloop,1, -1
        !read metallicity related variables
        
        if (i == 2) then
            ZAMS_HE_EEP = -1
            TAMS_HE_EEP = -1
            GB_HE_EEP = -1
            TPAGB_HE_EEP = -1

            cCBurn_HE_EEP = -1
            post_AGB_HE_EEP = -1
            Initial_EEP_HE = -1
    
            Final_EEP_HE = -1
    
            if (verbose) print*, 'Reading naked helium star tracks'
            call get_metallcity_file_from_Z(initial_Z,metallicity_file_list_he,ierr)
            if (ierr/=0) then
                print*, "Switching to SSE formulae for helium stars "
                cycle
            endif
            USE_DIR = TRACKS_DIR_HE
        else
            if (verbose) print*, 'Reading main (hydrogen star) tracks'
            call get_metallcity_file_from_Z(initial_Z,metallicity_file_list,ierr)
            if (ierr/=0) STOP
            USE_DIR = TRACKS_DIR
        endif
        
        ! check if the format file exists
        inquire(file=trim(format_file), exist=res)
        
        if ((res .eqv. .False.) .and. (front_end == COSMIC)) then
            if (debug) print*, trim(format_file), 'not found; appending ',trim(USE_DIR)
            format_file = trim(USE_DIR)//'/'//trim(format_file)
        endif
    
        !read file-format
        call read_format(format_file,ierr); if (ierr/=0) STOP
            
        !get filenames from input_files_dir
        
        if (trim(INPUT_FILES_DIR) == '' )then
            print*,"Error: INPUT_FILES_DIR is not defined for Z= ", initial_Z
            STOP
        endif
        
        if (verbose) print*,"Reading input files from: ", trim(INPUT_FILES_DIR)

        if (front_end == COSMIC) then
            find_cmd = 'find '//trim(INPUT_FILES_DIR)//'/*'//trim(file_extension)//' -maxdepth 1 > .file_name.txt'

            call execute_command_line(find_cmd,exitstat=ierr,cmdstat=jerr,cmdmsg=rnd)
            if (ierr/=0) then
            if (debug)print*, trim(find_cmd), 'not found; appending ',trim(USE_DIR)
            INPUT_FILES_DIR = trim(USE_DIR)//'/'//trim(INPUT_FILES_DIR)
            ierr =0
            endif
        endif
        
        call get_files_from_path(INPUT_FILES_DIR,file_extension,track_list,ierr)
        
        if (ierr/=0) then
            print*,'Error: failed to read input files.'
            print*,'Check if INPUT_FILES_DIR is correct.'
            STOP
        endif

        num_tracks = size(track_list)
        if (verbose) print*,"Number of input tracks: ", num_tracks
        allocate(xa(num_tracks))
        xa% filename = track_list
        set_cols = .true.
        
        
        if (i == 2) then
            xa% is_he_track = .true.
            call read_key_eeps_he()
            if (debug) print*, "key he eeps", key_eeps_he
        else
            xa% is_he_track = .false.
            call read_key_eeps()
            if (debug) print*, "key eeps", key_eeps
        endif
        
        if (read_eep_files) then
            if (debug) print*,"reading eep files"
            do j=1,num_tracks
                call read_eep(xa(j))
                if(debug) write(*,'(a100,f8.2,99i8)') trim(xa(j)% filename), xa(j)% initial_mass, xa(j)% ncol
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

            do j=1,num_tracks
                call read_input_file(xa(j))
                if(debug) write(*,'(a100,f8.2,99i8)') trim(xa(j)% filename), xa(j)% initial_mass, xa(j)% ncol
            end do
        endif
        
        ! Processing the input tracks
        if (i==2) then
            call set_zparameters_he()
            call copy_and_deallocatex(sa_he)
            call get_minmax(sa_he,Mmax_he_array,Mmin_he_array)

            use_sse_NHe = .false.
            allocate(core_cols_he(4))
            core_cols_he = -1
            core_cols_he(1) = i_he_age
            core_cols_he(2) = i_logL
            core_cols_he(3) = i_co_core
            if (i_he_RCO > 0) core_cols_he(4) = i_he_RCO
        else
           
            !reset z parameters where available
            !and determine cutoff masses
            call set_zparameters(zpars)
            call copy_and_deallocatex(sa)
            
            call get_minmax(sa,Mmax_array,Mmin_array)

            allocate(core_cols(6))
            core_cols = -1
            
            core_cols(1) = i_age
            core_cols(2) = i_logL
            core_cols(3) = i_he_core
            core_cols(4) = i_co_core

            if (i_RHe_core > 0) core_cols(5) = i_RHe_core
            if (i_RCO_core > 0) core_cols(6) = i_RCO_core
        endif
        deallocate(track_list)
    end do
!    if(debug) print*, s(1)% cols% name, s(1)% tr(:,1)

    !TODO: 1. check for monotonicity of initial masses
    ! 2. incompleteness of the tracks
    ! 3. BGB phase
    if (debug) print*,sa% initial_mass
    
    if (front_end == main) then
    ! sets remnant schmeme from SSE_input_controls
        call assign_commons_main()
    else
    ! reads
        call assign_commons()
    endif

end subroutine METISSE_zcnsts

