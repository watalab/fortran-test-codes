PROGRAM grade_evaluation
  IMPLICIT NONE
  INTEGER :: absences, report_score, exam_score
  REAL :: total_score, weighted_score
  CHARACTER(LEN=10) :: grade

  ! 入力: 欠席回数, レポート点, 試験点
  PRINT *, 'Enter absences, report score, exam score:'
  READ *, absences, report_score, exam_score

  ! 欠席回数が5回以上なら即不可
  IF (absences >= 5) THEN
    grade = '不可'
  ELSE
    ! 合計点の計算
    weighted_score = report_score * 0.3 + exam_score * 0.7
    total_score = MAX(weighted_score, REAL(exam_score))

    ! 成績判定
    IF (total_score >= 90) THEN
      grade = '秀'
    ELSE IF (total_score >= 80) THEN
      grade = '優'
    ELSE IF (total_score >= 70) THEN
      grade = '良'
    ELSE IF (total_score >= 60) THEN
      grade = '可'
    ELSE
      grade = '不可'
    END IF
  END IF

  ! 結果の出力
  PRINT *, 'Grade:', grade
END PROGRAM grade_evaluation
