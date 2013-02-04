program main
    implicit none
integer(4) i,j,k,kk
integer(4) periodik
real(8) ddir
real(8) lobsh
real(8) lmemb
real(8) hmemb
real(8),allocatable:: memax(:),memay(:),memaz(:)
character(2) me
integer(4) N,nom
integer(4) hperiodik

me='A'
periodik=8
ddir=1.15
hmemb=float(periodik)*ddir
lobsh=20.0
lmemb=10.0
hperiodik=3

allocate(memax(2*periodik*periodik*hperiodik))
allocate(memay(2*periodik*periodik*hperiodik))
allocate(memaz(2*periodik*periodik*hperiodik))

nom=0
N=2*periodik*periodik*hperiodik
do i=1,2
    do j=1,periodik
        do k=1,periodik
            do kk=1,hperiodik
            nom=nom+1
            memax(nom)=(float(j)-0.5)*ddir
            memay(nom)=(float(k)-0.5)*ddir
            memaz(nom)=(float(i)-1.5)*lmemb+(float(kk)-1.0)*ddir
            enddo
        enddo
    enddo
enddo
maxzdob=0.5*lmemb+(float(hperiodik)-1.0)*ddir+0.5*ddir



open(7,file='membrane.gro')
write(7,'(a)') 'membrane created with memcreat'
write(7,'(i5)') N
do i=1,N
    write(7,'(i5,2a5,i5,3f8.3,3f8.4)') i, 'MEM', me,1,memax(i),memay(i),memaz(i),0.0,0.0,0.0
enddo
write(7,'(3f8.4)') hmemb,hmemb,lobsh
close(7)


end program main
