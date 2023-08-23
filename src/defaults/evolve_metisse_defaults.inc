! Defaults control for METISSE
! Do NOT make any changes to this file
! Use evolve_metisse.in for input

! NOTE: SSE_input_controls in evolve_metisse.in are read 
! only when METISSE is called directly through its main unit/ standalone mode.
! Otherwise, values provided by the overlying code are used.
! Make sure to ADD a new line at the end (after / symbol)

    METISSE_DIR = '.'
    
!SSE_input_controls

  ! EVOLUTION CONTROLS
   
    ! For reading input masses for new tracks
    read_mass_from_file = .false.      

    input_mass_file = ''

    number_of_tracks = 0

    max_mass = -1.0

    min_mass = -1.0
    
    initial_Z = -1.0

    ! Maximum age in Myrs
    max_age = -1.0   

  ! REMNANT CONTROLS


    ! Options - "Mestel", "Modified_mestel"
    WD_mass_scheme = 'Modified_mestel'

    
    ! Only for White Dwarfs
    Use_initial_final_mass_relation = .false.       


    ! Options - "original_SSE", "Belczynski2002", "Belczynski2008", "Eldridge_Tout2004"
    BHNS_mass_scheme = 'Belczynski2008'

    
    ! Maximum neutron star mass 
    ! Suggested 1.8 for BHNS_mass_scheme="original_SSE", 3.0 otherwise

    Max_NS_mass = 3.d0
   
    ! Allow electron capture supernovae

    allow_electron_capture = .true.       
    
  ! TIMESCALE CONTROLS

    pts_1 = 0.05
    pts_2 = 0.01
    pts_3 = 0.02

  !OUTPUT CONTROLS

    ! 'write_track_to_file' generates a SSE style output file 
    ! only at the END of the evolution

    write_track_to_file = .true.

!METISSE_input_controls
    
    ! A metallicity file contains details about 
    ! the set of input tracks for a given metallicity,
    ! such as the path to the folder, their metallicity value
    ! and other information/metadata (see metallicity_defaults.in)

    ! The option 'metallicity_file_list' is used for providing 
    ! path/location of that metallicity file.
    ! In the case of a grid of stellar tracks,
    ! with folders containing tracks of various metallicities,
    ! location of the metallicity file for each folder/metallicity
    ! can be provided as a list of comma-separated strings 
    ! for up to 20 metallicities.
    ! For example: metallicity_file_list = 'path1',
    !					                             'path2',
    !                                       ...
    !					                             'path20'

    
    metallicity_file_list = ''


    ! if (abs(Z_input-Z_required)/MIN(Z_input,Z_required)) > Z_accuracy_limit
    Z_accuracy_limit = 1d-2

    ! INTERPOLATION CONTROLS

    ! Skip interpolation in mass if there is already
    ! an input track with initial_mass within the 'mass_accuracy_limit'

    mass_accuracy_limit = 1d-4

    ! OTHER REMNANT CONTROLS
    ! If true, 'construct_wd_track' is used (for low-mass stars) to construct the track between 
    ! Thermally-Pulsating AGB phase or tip of the AGB to the white dwarf cooling track
    ! It is useful if input tracks do not contain this phase
    ! but can be used otherwise too.

    construct_wd_track = .true.

    
    ! OUTPUT CONTROLS

    ! if true, 'verbose' prints useful details when reading the files

    verbose = .false. 

    ! 'write_eep_file' generates MIST style output file 
    ! at EVERY step of mass interpolation
    ! useful for debugging and single star evolution calculations with implicit mass loss

    write_eep_file = .false.		

  



