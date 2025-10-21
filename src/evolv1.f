***
      SUBROUTINE evolv1(kw,mass,mt,r,lum,mc,rc,menv,renv,ospin,
     &                  epoch,tm,tphys,tphysf,dtp,z,zpars)
c-------------------------------------------------------------c
c
c     Evolves a single star.
c     Mass loss is an option.
c     The timestep is not constant but determined by certain criteria.
c     Plots the HRD and variables as a function of time.
c
c     Written by Jarrod Hurley 26/08/97 at the Institute of
c     Astronomy, Cambridge.
c
c-------------------------------------------------------------c
c
c     STELLAR TYPES - KW
c
c        0 - deeply or fully convective low mass MS star
c        1 - Main Sequence star
c        2 - Hertzsprung Gap
c        3 - First Giant Branch
c        4 - Core Helium Burning
c        5 - First Asymptotic Giant Branch
c        6 - Second Asymptotic Giant Branch
c        7 - Main Sequence Naked Helium star
c        8 - Hertzsprung Gap Naked Helium star
c        9 - Giant Branch Naked Helium star
c       10 - Helium White Dwarf
c       11 - Carbon/Oxygen White Dwarf
c       12 - Oxygen/Neon White Dwarf
c       13 - Neutron Star
c       14 - Black Hole
c       15 - Massless Supernova
c
c-------------------------------------------------------------c
      implicit none
*
      integer kw,it,ip,jp,j,kwold,rflag
      integer nv
      parameter(nv=50000)
*
      REAL*8 mass0(1)
      real*8 mass,z,aj
      real*8 epoch,tphys,tphys2,tmold,tbgold
      real*8 mt,tm,tn,tphysf,dtp,tsave
      real*8 tscls(20),lums(10),GB(10),zpars(20)
      real*8 r,lum,mc,teff,rc,menv,renv,vs(3),mcx
      real*8 ospin,jspin,djt,djmb,k2,k3,wind
      parameter(k3=0.21d0)
      real*8 m0,mt0,r1,lum1,mc1,rc1,menv1,renv1,k21
      real*8 dt,dtm,dtr,dr,dtdr,dms,dml,mt2,rl
      real*8 tol,tiny
      parameter(tol=1.0d-10,tiny=1.0d-14)
      real*8 ajhold,rm0,eps,alpha2
      parameter(eps=1.0d-06,alpha2=0.09d0)
      real*8 mlwind,vrotf
      external mlwind,vrotf
      logical iplot,isave,dbg
      REAL*8 neta,bwind,hewind,mxns
      COMMON /VALUE1/ neta,bwind,hewind,mxns
      REAL*8 pts1,pts2,pts3
      COMMON /POINTS/ pts1,pts2,pts3
      REAL scm(50000,15),spp(20,3)
      COMMON /SINGLE/ scm,spp
      INTEGER irecord
      COMMON /REC/ irecord
      integer id
      INTEGER SSE_FLAG
      COMMON /SE/ SSE_FLAG
*
      ! in evolv1 hrdiag can be called several times
      !with METISSE not everytime the calculations needs to be saved in the pars array
      !so irecord is used to hrdiag when to save values
      irecord = 0
      id = 1
      mass0(1) = mass
      if (sse_flag/=0) call allocate_track(1,mass0)
      
      dtm = 0.d0
      r = 0.d0
      lum = 0.d0
      mc = 0.d0
      mc1 = 0.d0
      rc = 0.d0
      rl = 0.d0
      wind = 0.d0
    
      if(ospin.le.0.d0)then
         ospin = 1.0d-10
         jspin = 1.0d-10
      endif
      k2 = 0.15d0
      rflag = 0
*
* Setup variables which control the output (if it is required).
*
      dbg = .false. !this controls screen output
      ip = 0
      jp = 0
      tsave = tphys
      isave = .true.
      iplot = .false.
      if(dtp.le.0.d0)then
         iplot = .true.
         isave = .false.
         tsave = tphysf
      elseif(dtp.gt.tphysf)then
         isave = .false.
         tsave = tphysf
      endif
* 
      do 10 , j = 1,nv
*
         if(neta.gt.tiny.and.j.gt.1)then
*
* Calculate mass loss from the previous timestep.
*
            dt = 1.0d+06*dtm
            wind = mlwind(kw,lum,r,mt,mc,rl,z,id)
            dms = wind*dt
            if(kw.lt.10)then
               dml = mt - mc
               if(dml.lt.dms)then
                  dtm = (dml/dms)*dtm
                  dms = dml
                  if (dtm <= 0.d0) write(*,*)' DT3 ',dtm,dml,mc,mt
               endif
            endif
         else
            dms = 0.d0
         endif
*        if (kw>3 .and. kw <5) write(*,*)' DT3 ',dtm,dms,mt
*
* Limit to 1% mass loss.
*
         if(dms.gt.0.01d0*mt)then
            dtm = 0.01d0*mt*dtm/dms
            dms = 0.01d0*mt
*            if (kw>3 .and. kw <5) write(*,*)' DT4 ',dtm,dms,mt
         endif

*
* Calculate the rate of angular momentum loss due to magnetic braking 
* and/or mass loss.
*
         if(j.gt.1)then
            djt = (2.d0/3.d0)*(dms/(1.0d+06*dtm))*r*r*ospin
            if(mt.gt.0.35d0.and.kw.lt.10)then
               djmb = 5.83d-16*menv*(r*ospin)**3/mt
               djt = djt + djmb
            endif
         endif
*
* Update mass and time and reset epoch for a MS (and possibly a HG) star.
*
         if(dms.gt.0.d0)then
            mt = mt - dms
            if(kw.le.2.or.kw.eq.7)then
               m0 = mass
               mc1 = mc
               mass = mt
               tmold = tm
               tbgold = tscls(1)

               if(dbg)print*,"star1:update epoch",kw,aj,dtm,tphys
               CALL star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm
     &                          ,id)

                if(kw.eq.2)then
                  if (sse_flag==0) then
                  if(GB(9).lt.mc1.or.m0.gt.zpars(3))then
                     mass = m0
*                    print*, "I came here 2.1", tphys,epoch
                  else
                     epoch = tm + (tscls(1) - tm)*(ajhold-tmold)/
     &                            (tbgold - tmold)
                     epoch = tphys - epoch
*                     if(dbg)print*, "I came here 2.2",tphys,epoch
                 endif
                 endif
               else
                  epoch = tphys - ajhold*tm/tmold
*                  print*, "I came here 1", tphys,epoch,tm,tmold
               endif
            endif
         endif

         tphys2 = tphys
         tphys = tphys + dtm
*
* Find the landmark luminosities and timescales as well as setting
* the GB parameters.
*
         aj = tphys - epoch
         if(dtm<0) print*,"dtm<0:tphys, aj,epoch, kw"
         if(dtm<0) print*,tphys, aj,epoch, kw

         if(kw<10 .and. dbg)print*,"star2:main call",aj, mt, tphys, 
     &        epoch,dtm,kw
         CALL star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm,id)

*         if (kw<10)print*, tm, tscls(1), aj, mass, epoch , kw
*
* Find the current radius, luminosity, core mass and stellar type
* given the initial mass, current mass, metallicity and age
*
*         if (kw<10)print*,"calling hrdiag aj,mt,kw",aj,mt,kw,dtm,mc
         kwold = kw
         CALL hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &               r,lum,kw,mc,rc,menv,renv,k2,mcx,id)
*         if (kw>3 .and. kw<5) print*,"after hrdiag",aj,mt,kw,dtm,mc
*        write(50,*)j,kw,tphys,mass,mt,mc,aj,tscls(2)+tscls(3)
*        write(50,*)j,kw,tphys,mass,mt,mc,aj,tn
*
* If mass loss has occurred and no type change then check that we
* have indeed limited the radius change to 10%.
*
         if(kw.eq.kwold.and.dms.gt.0.d0.and.rflag.ne.0)then
            mt2 = mt + dms
            dml = dms/dtm
            it = 0
 20         dr = r - rm0
            if(ABS(dr).gt.0.1d0*rm0)then
               it = it + 1
               if(it.eq.20.and.kw.eq.4) goto 30
               if(it.gt.30)then
                  WRITE(99,*)' DANGER1! ',it,kw,mass,dr,rm0
                  WRITE(*,*)' STOP: EVOLV1 FATAL ERROR '
                  CALL exit(0)
                  STOP 
               endif
               dtdr = dtm/ABS(dr)
               dtm = alpha2*MAX(rm0,r)*dtdr
               if(it.ge.20) dtm = 0.5d0*dtm
               if(dtm.lt.1.0d-07*aj) goto 30
               dms = dtm*dml
               mt = mt2 - dms
               if(kw.le.2.or.kw.eq.7)then
                  mass = mt
                  if (kw<10) print*,"star 3: updating epoch",aj, mt,kw
                  CALL star(kw,mass,mt,tm,tn,tscls,lums,GB,
     &                          zpars,dtm,id)
                  if(kw.eq.2)then
                     if (sse_flag==0) then
                     if(GB(9).lt.mc1.or.m0.gt.zpars(3))then
                        mass = m0
                     else
                        epoch = tm + (tscls(1) - tm)*(ajhold-tmold)/
     &                               (tbgold - tmold)
                        epoch = tphys2 - epoch
                     endif
                     endif
                  else
                     epoch = tphys2 - ajhold*tm/tmold
                  endif
               endif
               tphys = tphys2 + dtm
               aj = tphys - epoch
               mc = mc1
               if (kw<10) print*,"star 4: after updating epoch",aj,mt,kw
               CALL star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm
     &                          ,id)
               CALL hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &                     r,lum,kw,mc,rc,menv,renv,k2,mcx,id)
*           print*, "aj2",aj,dtm
               goto 20
            endif
 30         continue
         endif
*
* Initialize or adjust the spin of the star.
*
         if(j.eq.1)then
            if(tphys.lt.tiny.and.ospin.lt.0.001d0)then
               ospin = 45.35d0*vrotf(mt)/r
            endif
            jspin = ospin*(k2*r*r*(mt-mc)+k3*rc*rc*mc)
         else
            jspin = MAX(1.0d-10,jspin - djt*1.0d+06*dtm)
            ospin = jspin/(k2*r*r*(mt-mc)+k3*rc*rc*mc)
         endif
*
* Test for changes in evolution type.
*
         if(j.eq.1.or.kw.ne.kwold)then
            if(kw.ge.10.and.kw.le.14)then
               CALL kick(kw,mass,mt,0.d0,0.d0,-1.d0,0.d0,vs)
            endif
*
* Force new NS or BH to have a one second period. 
* 
            if(kw.eq.13.or.kw.eq.14)then
               ospin = 2.0d+08
               jspin = k3*rc*rc*mc*ospin
            endif
            jp = jp + 1
            spp(jp,1) = tphys
            spp(jp,2) = float(kw)
            if(kw.eq.15)then
               spp(jp,3) = mass 
               goto 90
            else
               spp(jp,3) = mt
            endif
         endif
*
* Record values for plotting and reset epoch.
*
* added by Poojan-  another call to hrdiag to store current values in pars structure
         irecord = 1
         CALL hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &                     r,lum,kw,mc,rc,menv,renv,k2,mcx,id)
         irecord = 0
         epoch = tphys - aj
         if((isave.and.tphys.ge.tsave).or.iplot)then
            ip = ip + 1
            scm(ip,1) = tphys
            scm(ip,2) = float(kw)
            scm(ip,3) = mass
            scm(ip,4) = mt
            scm(ip,5) = log10(lum)
            scm(ip,6) = log10(r)
            teff = 1000.d0*((1130.d0*lum/(r**2.d0))**(1.d0/4.d0))
            scm(ip,7) = log10(teff)
            scm(ip,8) = mc
            scm(ip,9) = rc
            scm(ip,10) = menv
            scm(ip,11) = renv
            scm(ip,12) = epoch
            scm(ip,13) = ospin
            scm(ip,14) = mcx
            scm(ip,15) = wind
            if(isave) tsave = tsave + dtp
            if(tphysf.lt.tiny)then
               ip = ip + 1
               do 35 , it = 1,15
                  scm(ip,it) = scm(ip-1,it)
 35            continue
            endif
         endif
*
         if(tphys.ge.tphysf)then
            jp = jp + 1
            spp(jp,1) = tphys
            spp(jp,2) = float(kw)
            spp(jp,3) = mt
            goto 90
         endif
*
* Record radius and current age.
*
         rm0 = r
         ajhold = aj
         if(kw.ne.kwold) kwold = kw
         CALL deltat(kw,aj,tm,tn,tscls,dtm,dtr,id)
*        write(50,*)' DT1 ',dtm,dtr,dtr-dtm,aj,tscls(2)+tscls(3)
*        if (kw>1 .and.kw<10) write(*,*)' DT1 ',dtm,dtr,aj,tn,kw
*
* Check for type change.
*
         it = 0
         m0 = mass
         mt0 = mt
         if((dtr-dtm).le.tol.and.kw.le.9)then
*
* Check final radius for too large a jump.
*
            aj = MAX(aj,aj*(1.d0-eps)+dtr)
            mc1 = mc 
            CALL hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &        r1,lum1,kw,mc1,rc1,menv1,renv1,k21,mcx,id)
*            print*, "aj3",aj,dtm
            dr = r1 - rm0
*           write(*,*)' check final ',dr
            if(ABS(dr).gt.0.1d0*rm0.and.kw.lt.10)then
               dtm = dtr - ajhold*eps
               dtdr = dtm/ABS(dr)
               dtm = alpha2*MAX(r1,rm0)*dtdr
               goto 40
            else
               dtm = dtr
               goto 50
            endif
         endif
*
* Limit to a 10% increase in radius assuming no further mass loss
* and thus that the pertubation functions due to small envelope mass
* will not change the radius.
*
*         if (kw<10) print*, "new cycle"
 40      aj = ajhold + dtm
         mc1 = mc
         if (kw<10 .and. dbg) print*, "before aj4",aj,dtm,kw
         CALL hrdiag(mass,aj,mt,tm,tn,tscls,lums,GB,zpars,
     &               r1,lum1,kw,mc1,rc1,menv1,renv1,k21,mcx,id)
*          if (kw<10) print*, "after aj4,dtm,kw",aj,dtm,kw
         dr = r1 - rm0
         it = it + 1
         if(it.eq.20.and.kw.eq.4) goto 50
         if(kw.ne.kwold.and.kw.ge.13) goto 50
*        if(it.eq.20.and.kw.gt.9) goto 50
         if(it.gt.30)then
            WRITE(99,*)' DANGER2! ',it,kw,mass,dr,rm0
            WRITE(*,*)' STOP: EVOLV1 FATAL ERROR '
            CALL exit(0)
            STOP 
         endif
         if(ABS(dr).gt.0.1d0*rm0)then
            dtdr = dtm/ABS(dr)
            dtm = alpha2*MAX(rm0,r1)*dtdr
            if(it.ge.20) dtm = 0.5d0*dtm
*            print*, "rad check",dtm,dr, it,kw
            goto 40
         endif
*
 50      continue
*
* Ensure that change of type has not occurred during radius check. 
* This is rare but may occur for HG stars of ZAMS mass > 50 Msun 
* or some cases at the upper end of the mass range for EC SNe. 
*
         if(kw.ne.kwold)then
            if(kw<10.and.dbg)print*,"star5:phase change",aj,mt,mc,kw,dtm
            kw = kwold
            mass = m0
            mt = mt0
            CALL star(kw,mass,mt,tm,tn,tscls,lums,GB,zpars,dtm,
     &                          id)
*            if (kw<10) print*,"after star5:phase change",aj, mt,mc,kw
         endif
*
* Choose minimum of time-scale and remaining interval (> 100 yrs).
*
         dtm = MAX(dtm,1.0d-07*aj)
         dtm = MIN(dtm,tsave-tphys)
         if (dtm<0) then
         print*,"negative timestep",dtm
         STOP
         endif

*        write(*,*)' DT2 ',dtm,dtr,1.0d-07*aj,tsave-tphys,kw
*         if (kw>1 .and. kw <10) write(*,*)' DT2 ',dtm,dtr,1.0d-07*aj

 10   continue
*
 90   continue
*
      tphysf = tphys
      scm(ip+1,1) = -1.0
      spp(jp+1,1) = -1.0
      if(ip.ge.nv)then
         WRITE(99,*)' EVOLV1 ARRAY ERROR ',mass
         WRITE(*,*)' STOP: EVOLV1 ARRAY ERROR '
         CALL exit(0)
         STOP
      endif
*
      if (sse_flag/=0) call dealloc_track()
      RETURN
      END
***
