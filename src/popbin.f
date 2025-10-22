***
      PROGRAM popbin
***
*
* Evolves a population of binaries using input parameters 
* read from input file binaries.in (M1, M2, P, e, Z, Tmax). 
*
***
      implicit none
*
      INCLUDE 'const_bse.h'
*
      integer i,j,k,jj,nm1
      integer kw,kw2,kwx,kwx2,kstar(2)
      integer i1,i2,kdum,k1,k2,bkw,ierr
*
      real*8 m1,m2,tmax,age1
      real*8 mass0(2),mass(2),z,zpars(20)
      real*8 epoch(2),tms(2),tphys,tphysf,dtp
      real*8 rad(2),lum(2),ospin(2)
      real*8 massc(2),radc(2),menv(2),renv(2)
      real*8 sep0,tb0,tb,ecc0,ecc,aursun,yeardy,yearsc,tol
      PARAMETER(aursun=214.95d0,yeardy=365.25d0,yearsc=3.1557d+07)
      PARAMETER(tol=1.d-07)
      real*8 t1,t2,mx,mx2,tbx,eccx,a
      CHARACTER*256 fname

*
************************************************************************
* BSE parameters:
*
* neta is the Reimers mass-loss coefficent (neta*4x10^-13: 0.5 normally). 
* bwind is the binary enhanced mass loss parameter (inactive for single).
* hewind is a helium star mass loss factor (1.0 normally).
* alpha1 is the common-envelope efficiency parameter (1.0).  
* lambda is the binding energy factor for common envelope evolution (0.5).
*
* ceflag > 0 activates spin-energy correction in common-envelope (0). 
* tflag > 0 activates tidal circularisation (1).
* ifflag > 0 uses WD IFMR of HPE, 1995, MNRAS, 272, 800 (0). 
* wdflag > 0 uses modified-Mestel cooling for WDs (0). 
* bhflag > 0 allows velocity kick at BH formation (0). 
* nsflag > 0 takes NS/BH mass from Belczynski et al. 2002, ApJ, 572, 407 (1). 
* mxns is the maximum NS mass (1.8, nsflag=0; 3.0, nsflag=1). 
* idum is the random number seed used by the kick routine. 
*
* Next come the parameters that determine the timesteps chosen in each
* evolution phase:
*                 pts1 - MS                  (0.05) 
*                 pts2 - GB, CHeB, AGB, HeGB (0.01)
*                 pts3 - HG, HeMS            (0.02)
* as decimal fractions of the time taken in that phase.
*
* sigma is the dispersion in the Maxwellian for the SN kick speed (190 km/s). 
* beta is wind velocity factor: proportional to vwind**2 (1/8). 
* xi is the wind accretion efficiency factor (1.0). 
* acc2 is the Bondi-Hoyle wind accretion factor (3/2). 
* epsnov is the fraction of accreted matter retained in nova eruption (0.001). 
* eddfac is Eddington limit factor for mass transfer (1.0).
* gamma is the angular momentum factor for mass lost during Roche (-1.0). 
* sse_flag is the flag to use metisse (1) or sse(0) as the stellar engine (1 default)
*
*      neta = 0.5
*      bwind = 0.0
*      hewind = 1.0
*      alpha1 = 5.0
*      lambda = 0.1
*      ceflag = 0
*      tflag = 1
*      ifflag = 0
*      wdflag = 1
*      bhflag = 0
*      nsflag = 2 !this is different to MOBSE/NBODY6++GPU
*      mxns = 3.0
*      pts1 = 0.05
*      pts2 = 0.01
*      pts3 = 0.02
*      sigma = 15.0
*      beta = 0.125
*      xi = 1.0
*      acc2 = 1.5
*     epsnov = 0.001
*      eddfac = 1.0
*      gamma = -1.0
*
* Set the seed for the random number generator. 
*
*      idum = 3234

      OPEN(22,file='binary.in', status='old')
      READ(22,*)mass0(1),mass0(2),tphysf,tb,kstar(1),kstar(2),z,ecc
      READ(22,*)neta,bwind,hewind,alpha1,lambda
      READ(22,*)ceflag,tflag,ifflag,wdflag,bhflag,nsflag,mxns,idum
      READ(22,*)pts1,pts2,pts3
      READ(22,*)sigma,beta,xi,acc2,epsnov,eddfac,gamma
      READ(22,*)sse_flag
      CLOSE(22)

      if(idum.gt.0) idum = -idum
*
* Set the collision matrix.
*
      CALL instar
*
* Open the output files. 
*
      OPEN(11,file='binaries.out',status='old')
      WRITE(11,*)'  TIME     K1  K2    M1        M2       L1         ',
     &           'L2        R1        R2        T1        T2        ',
     &           'ECC     PORB          SEP'

*      OPEN(100,file='NaN_info.out',status='unknown')
*
* Open the binary input file - list of binary initial parameters.
*
      OPEN(10,file='binaries.in',action='read',status='old')
      READ(10,*)nm1, Z
*      READ(10,*)nm1

*
* Set parameters which depend on the metallicity
*      
      !set the front end for METISSE
      if(sse_flag/=0) call initialize_front_end('BSE')
      CALL zcnsts(z,zpars)
      do i = 1,nm1
*
* Read in parameters and set coefficients which depend on metallicity. 
*
         READ(10,*,iostat=ierr)k1,k2,m1,m2,tb,ecc,age1,tmax
*         READ(10,*,iostat=ierr)k1,k2,m1,m2,tb,ecc,z,age1,tmax

         write(99,*) 'ip',i,k1,k2,m1,m2,tb,ecc,tmax
         ecc0 = ecc
         tb0 = tb/yeardy
         sep0 = aursun*(tb0*tb0*(mass(1) + mass(2)))**(1.d0/3.d0)
         tb0 = tb
*
* Initialize the binary. 
*
         kstar(1) = k1
         mass0(1) = m1
         mass(1) = m1
         massc(1) = 0.0
         ospin(1) = 0.0
         epoch(1) = 0.0
*
         kstar(2) = k2
         mass0(2) = m2
         mass(2) = m2
         massc(2) = 0.0
         ospin(2) = 0.0
         epoch(2) = 0.0
*
         tphys = age1
         tphysf = tmax
         dtp = 0.0
*
* Evolve the binary. 
*
         CALL evolv2(kstar,mass0,mass,rad,lum,massc,radc,
     &               menv,renv,ospin,epoch,tms,
     &               tphys,tphysf,dtp,z,zpars,tb,ecc)
*
* Search the BCM array for the formation of binaries of 
* interest (data on unit 12 if detected) and also output 
* the final state of the binary (unit 11). 
*
* In this example we will search for CVs. 
*
         jj = 0
         t1 = -1.0
         t2 = -1.0
 30      jj = jj + 1
         if(bcm(jj,1).lt.0.0) goto 40
         
         kw = INT(bcm(jj,2))
         kw2 = INT(bcm(jj,16))
*
         
         goto 30
 40      continue
*
         jj = jj - 1
         kw = INT(bcm(jj,2))
         kw2 = INT(bcm(jj,16))
         mx = bcm(jj,4)
         mx2 = bcm(jj,18)
         tbx = bcm(jj,30)*yeardy   !tbx is in years- need it in days
         a = bcm(jj,31)     !separation in Rsun
         eccx = bcm(jj,32)

*         WRITE(*,*) 'Porb',tmax,kw,kw2,mx,mx2,eccx,tbx,a
*         WRITE(*,*) 'Porb2',jj,bcm(jj,4),bcm(jj,18),bcm(jj,30)

*         if (kw==15 .or. kw2==15) print*, a,kw,kw2,i
*         if (a/=a) then !checking for NaN
*            print*,'Nan',bcm(jj,31), a, i
*            print*, (kw==15),(kw2==15)
*            WRITE(100,110)tmax,kw,kw2,mx,mx2,eccx,tbx,a,i
            !STOP
*         endif
         WRITE(11,111)tmax,kw,kw2,mx,mx2,bcm(jj,5),bcm(jj,19),
     &                bcm(jj,6),bcm(jj,20),bcm(jj,7),bcm(jj,21),
     &                eccx,tbx,a
*
* Write bpp output to file
*
          j = 0
          write(fname,"(a,i5.5,a)")"output_popbin/bpp",i,".out"
          OPEN(123,file=fname,status='unknown')

          WRITE(123,*)'     TIME      M1       M2   K1 K2        SEP',
     &                '    ECC  R1/ROL1 R2/ROL2  TYPE'
52        j = j + 1
          if(bpp(j,1).lt.0.0) goto 60

          kstar(1) = INT(bpp(j,4))
          kstar(2) = INT(bpp(j,5))
          bkw = INT(bpp(j,10))
          if (bpp(j,6) >= 1e+8) bpp(j,6) = 9.99e+7
          WRITE(123,101)(bpp(j,k),k=1,3),kstar,(bpp(j,k),k=6,9),bkw
          goto 52
60        continue
          CLOSE(123)
      enddo
*
 101  FORMAT(f11.4,2f9.3,2i3,f13.3,f6.2,2f8.3,2x,i3)
 111  FORMAT(f10.1,2i4,9f10.4,2e14.6)
 110  FORMAT(f10.1,2i3,3f8.3,2f12.3,i10)
 112  FORMAT(3f8.3,1p,e14.6,0p,2f10.2,2i3,3f8.3,1p,e14.6)
      CLOSE(10)     !binaries.in
      CLOSE(11)     !binaries.out
*      CLOSE(100)
*
************************************************************************
*
      STOP
      END
***
