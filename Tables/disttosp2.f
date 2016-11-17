      Program disttosp
C calculates distance from dwarf to nearest spiral

      real*4 delr, delra, delra1, delra2, dang
      real*4 deldec, decm, ram, dfin 
      character	 name1*32, namef*32
      character*24	fdate
      REAL*4 DIS(50),rhs(50),rms(50),rss(50),dds(50),dms(50),dss(50)
      REAL*4 DID(50),rhd(50),rmd(50),rsd(50),ddd(50),dmd(50),dsd(50)


	PARAMETER (PI=3.14159265358)
C
C --- Creation of a logfile:
C
	OPEN(unit=21,file='disttosp.log',status='unknown')

C
C --- Initialize output:
C
	write(6,*)
	write(6,*) ' DISTTOSP Execution on ',fdate()
	write(6,*)

C
	if (iargc().eq.1) then
		call getarg(1,namef)
	else
		write (6,524) ' NAME of file with spirals ? '
		read (5,500) namef
	endif
	if (index(namef,'.').ne.0) then
		name1=namef(:index(namef,'.'))//'txt'
	else
		name1=namef(:lnblnk(namef))//'.txt'
	endif

524	format(1X,A29)
500	format(A32)

C
C --- Input parameters:
C --- Lecture des donnees : 
C
	OPEN(unit=11,file=name1,status='old',err=998)

	I=1
100	READ(11,*,end=110) dis(I),rhs(I),rms(I),rss(i),dds(i),dms(i),dss(i)
      decm=dms(i) + dss(i)/60.0
      dds(i) =dds(i) + decm/60.0
      ram= rms(i) + rss(i)/60.0
      rhs(i)= rhs(i) + ram/60.0

	I=I+1
	GO TO 100

110	NPTSS=I-1
	CLOSE(UNIT=11)
C
C
C
	if (iargc().eq.1) then
		call getarg(1,namef)
	else
		write (6,524) ' NAME of file with dwarfs ? '
		read (5,500) namef
	endif
	if (index(namef,'.').ne.0) then
		name1=namef(:index(namef,'.'))//'txt'
	else
		name1=namef(:lnblnk(namef))//'.txt'
	endif

C
C --- Input parameters:
C --- Lecture des donnees : 
C
	OPEN(unit=11,file=name1,status='old',err=998)

	I=1
200	READ(11,*,end=210)  did(I),rhd(I),rmd(I),rsd(i),ddd(i),dmd(i),dsd(i)
      decm=dmd(i) + dsd(i)/60.0
      ddd(i) =ddd(i) + decm/60.0
      ram= rmd(i) + rsd(i)/60.0
      rhd(i)= rhd(i) + ram/60.0


	I=I+1
	GO TO 200

210	NPTS=I-1
	CLOSE(UNIT=11)
C

        do 501, i=1,npts
         do 400, j=1,nptss

      deldec = abs(ddd(i)-dds(j))*60.0
      delra1= abs(rhd(i)-rhs(j))*15*cosd(ddd(i))
      delra2= abs(rhd(i)-rhs(j))*15*cosd(dds(j))
      delra= (delra1+delra2)/2.0*60.0

      delr= sqrt(delra**2 + deldec**2)

      scale1= PI/10.8 * dis(j)
      scale2= PI/10.8 * did(i)
      dang= (delr*scale1 + delr*scale2)/(2*1000.0)
      dfin= sqrt((dis(j)-did(i))**2 + dang**2)

      write(21,*) dfin, ' Mpc'
400      continue

       write(21,*) '    '

501     continue  
	close(21)

	STOP ' Disttosp ends successfully.'

998	write(6,*) ' ERROR OPENING FILE'
	STOP ' distosp Aborts!'
	END


