include 'FUNCTIONS.f90'

Program MARKS
   
   implicit none
   
   integer :: EoF
   integer, parameter :: N = 10
   character (len=500) :: line_soluciones
   character (len=8) :: DNI_soluciones
   real :: soluciones_correctas(N)
   character (len=2) :: nota_final
   character (len=500) :: line_grupos
   character (len=8) :: DNI_grupos
   
   open (UNIT=1, FILE="p1.TXT",STATUS="old")
   open (UNIT=4, FILE="Temp.csv", STATUS="new")
   ! open (UNIT=5, FILE="NOTASPERDIDAS.csv",STATUS="new")
  
   call calcular_soluciones('solutions.csv', 3, N, soluciones_correctas)
   
   do
      read (UNIT=1, IOSTAT=EoF, FMT='(A)') line_soluciones

      if(EoF < 0) then 
         exit
      endif

      DNI_soluciones = line_soluciones(1:8)

      call calcular_nota(soluciones_correctas, line_soluciones, N, nota_final) 
         
      open (UNIT=2, FILE='GRUPOS.csv', STATUS='old')
      read (UNIT=2, IOSTAT=EoF, FMT='(A)') line_grupos

      do
         read (UNIT=2, IOSTAT=EoF, FMT='(A)') line_grupos
         
         ! Notas perdidas
         if(EoF < 0) then
         !   write(UNIT=5,FMT='(A)') DNI_soluciones // ';' // nota_final
            close(UNIT=2)
            exit
         endif
         
         call obtener_dni(line_grupos, DNI_grupos)
            
         if(DNI_soluciones == DNI_grupos) then
            write(UNIT=4, FMT='(A)') line_grupos // ';' // nota_final
            close(UNIT=2)
            exit
         endif
            
      end do

   end do

   open (UNIT=2, FILE='GRUPOS.csv', STATUS='old')
   close(UNIT=2, status='delete')

   close(UNIT=1)
   close(UNIT=4)
   close(UNIT=5)

   call RENAME('Temp.csv','GRUPOS.csv')
end program
