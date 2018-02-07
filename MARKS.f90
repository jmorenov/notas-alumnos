include 'FUNCTIONS.f90'

Program MARKS
   
   implicit none
   
   integer :: EoF
   integer, parameter :: N = 10
   character (len=500) :: line
   character (len=8) :: DNI
   real :: CANSWER(N)
   character (len=2) :: CNOTA
   character (len=500) :: line2
   character (len=8) :: DNI3
   
   ! APERTURA DE ARCHIVOS UTILIZABLES
   open (UNIT=1, FILE="p1.TXT",STATUS="old")
   open (UNIT=4, FILE="NOTAP1.csv",STATUS="new")
   open (UNIT=5, FILE="NOTASPERDIDAS.csv",STATUS="new")
  
   ! RESPUESTAS CORRECTAS
   call calcular_soluciones('solutions.csv', 3, N, CANSWER)
   
   ! BUCLE PRINCIPAL
   do
      read (UNIT=1, IOSTAT=EoF, FMT='(A)') line

      if(EoF < 0) then 
         exit
      endif

      ! DNI
      DNI = line(1:8)
      
      ! COMPARACION Y EVALUACIÓN DEL ALUMNO
      call calcular_nota(CANSWER, line, N, CNOTA) 
      
      ! ASIGNACIÓN DE LA NOTA A LOS ALUMNOS
      ! LECTURA ARCHIVO GRUPOS.CSV
         
      open (UNIT=2, FILE="GRUPOS.csv",STATUS="old")
      read (UNIT=2, IOSTAT=EoF, FMT='(A)') line2
      
      do
         read (UNIT=2, IOSTAT=EoF, FMT='(A)') line2
         
         if(EoF < 0) then
            write(UNIT=5,FMT='(A)') DNI // ';' // CNOTA
            close(UNIT=2)
            exit
         endif
         
         call obtener_dni(line2, DNI3)
            
         if(DNI == DNI3) then
            write(UNIT=4, FMT='(A)') line2 // ';' // CNOTA
            close(UNIT=2)
            exit  
         endif
            
      end do

   end do
   
   close(UNIT=1)
   close(UNIT=4)
   close(UNIT=5)
end program