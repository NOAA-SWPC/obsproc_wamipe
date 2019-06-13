      program latlonqc
c checks for missing position information in BUFR tank/dump files
c  optionally rewrites the file w/ bad reports removed
c
c  26 Mar 2019 JWhiting original code
c 
c  Given an input BUFR file w/ expected CLAT(H)/CLON(H) values, this 
c   program removes those reports w/ missing lat/lon values & writes 
c   only those w/ good position info to a new output file.
c----

      implicit     none

      integer      LUNIN,LUNOUT
      integer      ntoss,nr,nrdum,msub,nmsub,iret,idate,i

      integer      IREADMG, IREADSB, ibfms
      real*8       GETBMISS, pcttoss

      character*8  ctyp, cval(5)
      character*80 cnem
      real*8       usr(20), rval(5)
      real*8       hdr(20)
      equivalence (cval(1),rval(1))
      logical      write_hdr
      data         write_hdr / .true. /
      real*8       bmiss
      real*8       clath,clonh
c====67=10========20========30========40========50========60========70=2

      call w3tagb('BUFR_LATLONQC',2019,086,0050,'EMC')

c     write(*,'(5x,a)') 'start of prog: latlonqc v19.03.26'

      write(*,'(5x,a)') 'Removing reports w/ missing lat/lon values'

      LUNIN=50
c     OPEN(LUNIN,FORM='UNFORMATTED')

      LUNOUT=51
c     OPEN(LUNOUT,FORM='UNFORMATTED')


      call OPENBF(LUNIN,'IN',LUNIN)
      call OPENBF(LUNOUT,'OUT',LUNIN)

      call DATELEN(10)                          ! set dates to 8-digit
      bmiss=GETBMISS()


c-- Loop thru messages

      ntoss=0  ! count of rpts w/ missing lat/lon
      nr=0  ; nrdum=0
      do while( IREADMG(LUNIN,ctyp,idate).EQ.0 )


c-- Check for proper message type and subtype

        if ( ctyp(:2) .ne. 'NC' ) then 
          write(*,'(5x,a)') ' *** fatal ERROR data type ',ctyp,
     &                        ' not supported - exiting'
c         closebf(lunout)
c         call err_exit()
          stop
        endif ! ctyp != DBUOY


c-- Check for "dummy" dump messages (w/ dump wallclock and center times)

        msub = nmsub(lunin)
        if (msub.eq.0 .and. nr.eq.0) then

          if (nrdum.eq.0) then 
            write(*,*)
            write(*,'(5x,a)')   'Input File'
            write(*,'(5x,a)')   '   Data Type: ' // trim(ctyp)
            write(*,'(5x,a,$)') '   Dump Time, Center (cycle): '
            write(*,6) idate/100,mod(idate,100)*100
          endif

          if (nrdum.eq.1) then 
            write(*,'(5x,a,$)') '   Dump Time, wall clock    : '
            write(*,6) idate/100,mod(idate,100)*100
          endif ! nrdum=1

          nrdum=nrdum+1
          cycle
        endif
    6   format(1x,i8,'.',i4.4)


c-- Loop thru subsets, checking for missing lat/lon

        do while (iREADSB(lunin) .eq. 0) 
          nr=nr+1

c - get lat & lon
          usr=bmiss ; cnem='CLON CLONH CLAT CLATH'
          call UFBINT(lunin,usr,20,1,iret,cnem)

          clonh=bmiss ; clath=bmiss
          clonh=usr(1) ; if ( ibfms(usr(2)).eq.0) clonh=usr(2)
          clath=usr(3) ; if ( ibfms(usr(4)).eq.0) clath=usr(4)


c -- filter rpts w/ missing lat/lon

          if (ibfms(clath).ne.0) then 
            ntoss=ntoss+1


c  -write out info on tossed reports

c  -columns header

            if ( write_hdr ) then 
              write(*,'(/,5x,a,$)') 'Reports with no position info'
              write(*,'(     a,$)') ' (& removed from output file):'
              write(*,'(/,5x,a,$)') '  rpt#      ymd.hm  '
              write(*,'(     a,$)') '       long       lat'  ! 5-digit
              write(*,'(     a,$)') '    stnID'
              write_hdr=.false.
            endif  ! write_hdr

c  -report # (index)

            write(*,'(/,3x,1i8,$)') nr


c  -get report time tag & print

            hdr=bmiss ; cnem='YEAR MNTH DAYS HOUR MINU'
            call UFBINT(lunin,hdr,20,1,iret,cnem)
            write(*,7) (int(hdr(i)),i=1,5)                     ! yyyymmdd.hhmm
    7       format (1x,i4,2i2.2,".",2i2.2,$)

c- print (missing) lat lon
            write(*,'( (1x,f10.5,1x,f9.5),$)') clonh,clath     ! 5-decimal

            rval=bmiss ; cnem='RPID'
            call UFBINT(lunin,rval, 5,1,iret,cnem)
        
            if ( ibfms(rval(1)) .eq. 1 ) cval(1)='<-MSNG->' 
            write(*,'(1x,a8,$)') trim(cval(1))

            write(*,'(a,$)') " <-- tossed"

          else ! clath not missing

c -- write out valid reports

            call openmb(lunout,ctyp,idate)
            call ufbcpy(lunin,lunout)
            call writsb(lunout)
          endif ! clath missing


        end do ! while ireadsb

      end do ! while ireadmg
      write(*,*) ! closing linefeed

      call closbf(lunout)

c-- report count stats

      write(*,'(/,5x,a,$)') "Summary stats:" 
      pcttoss=1d2*float(ntoss)/float(nr)

c     write(*,'(2(/,a,1x,i5),1x,"(",f8.2," %)")')
      write(*,'(2(/,6x,i5,1x,a),1x,"(",f6.2," %)")')
     $  nr,    ': total # input reports',
     $  ntoss, ': #rpts missing lat/lon',pcttoss


c     write(*,'(5x,a)') ; write(*,'(5x,a)') 'end of prog latlonqc'

      call w3tage('BUFR_LATLONQC')

      end ! program latlonqc
