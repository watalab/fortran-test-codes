program helloOpenMP
  !$ use omp_lib
  implicit none
  print *, "START"
  !$omp parallel
  print *, "Hello! N =", omp_get_num_threads(), " and I am ", omp_get_thread_num()
  !$omp end parallel
  print *, "END"
  end
