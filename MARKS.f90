include 'FUNCTIONS.f90'

Program MARKS
   
   implicit none
   
   ! VARIABLES DE GUERRA
   
   integer :: i,j !INDEX
   integer :: EoF,FoE !IOSTAT
   integer, parameter :: N = 10
   real :: num

   character (len=500) :: line
   
   ! DNI
   character (len=8) :: DNI !DNI caracteres
   character (len=8) :: DNI2 !DNI numero
   
   ! BUCLE RESPUESTAS - NOTA
   real :: CANSWER(N)
   character (len=2) :: CNOTA
   
   ! BUCLE 2º DNI
   integer :: K !ENTERO PARA CONTAR LAS ';'
   character (len=1) :: pyc !PUNTO Y COMA
   character (len=500) :: line2 !line archivo 2
   
   character (len=8) :: DNI3
   
   ! APERTURA DE ARCHIVOS UTILIZABLES
   open (UNIT=1,FILE="p1.TXT",STATUS="old")
   open (UNIT=4,FILE="NOTAP1.csv",STATUS="new")
   open (UNIT=5,FILE="NOTASPERDIDAS.csv",STATUS="new")
  
   ! RESPUESTAS CORRECTAS
   call calcular_soluciones('solutions.csv', 3, N, CANSWER)
   
   ! BUCLE PRINCIPAL
   EoF = 0
   do

      if(EoF < 0) then 
         exit
      endif
      
      read (UNIT=1, IOSTAT=EoF, FMT='(A)') line

      ! DNI
      
      DNI = line(1:8)
      read(DNI,'(A)') DNI2
      
      ! COMPARACION Y EVALUACIÓN DEL ALUMNO
      call calcular_nota(CANSWER, line, N, CNOTA) 
      
      ! ASIGNACIÓN DE LA NOTA A LOS ALUMNOS
      ! LECTURA ARCHIVO GRUPOS.CSV
         
      open (UNIT=2,FILE="GRUPOS.csv",STATUS="old")
      read (UNIT=2, IOSTAT=FoE, FMT='(A)') line2
      
      do
         read (UNIT=2, IOSTAT=FoE, FMT='(A)') line2
         
         if(FoE < 0) then
         
            write(UNIT=5,FMT='(A)') DNI // ';' // CNOTA
            
            close(UNIT=2)
            
            exit
            
         endif
         
            K=0 !CONTADOR DE ';'
            i=1 !EL VALOR FINAL DE 'i' A LA SALIDA DEL PRIMER BUCLE SERÁ LA POSICIÓN DE LA 3º ';' EN LA LINEA
         
            do while(K<=2)
         
               pyc = line2(i:i)
         
               if(pyc == ';') then
                  K=K+1
               end if
            
               i=i+1
            end do
         
            j=i !ENCADENAMOS EL ÚLTIMO VALOR DE 'i' CON EL PRIMERO DE 'j' PARA QUE CONTINUE EN LA MISMA PARTE DE LA LINEA
         
            do while(K<=3)
         
               pyc = line2(j:j)
         
               if(pyc == ';') then
                  K=K+1
                  
                  if(k==3) then
                     exit
                  endif
                  
               endif
            
               j=j+1
            end do
             
            j=j-2
         
            DNI3 = line2(i:j)
            
            if(DNI2 == DNI3) then
            
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