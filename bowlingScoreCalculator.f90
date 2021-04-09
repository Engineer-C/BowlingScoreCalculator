module publicVariables
    implicit none
    integer :: indexing, frameNumber, ballNumber
    integer, dimension(10) :: frameScore = 0
    logical, dimension(10) :: frameScoreCalculated = .false.
    integer, dimension(21) :: scoreBoard = 0
    logical, dimension(21) :: ballThrow = .false.

end module

program BowlingScore
    use publicVariables
    call cpu_time(start)
    print '(A)', '<<--     GAME START     -->>'
    do indexing=1,21
        call pinControlAndScoring
    end do
    print '(A)', '<<--     END OF GAME    -->>'
    call cpu_time(finish)
    print "(a, f8.5, a)", 'Elapsed time:', (finish-start)*1000, ' ms.'
end program BowlingScore


subroutine frameBallNumber
    use publicVariables

    frameNumber = ceiling(real(indexing)/2.)
    if (mod(real(indexing),2.)==1) then
        ballNumber = 1
    else
        ballNumber = 2
    end if

    if (frameNumber == 11) then
        frameNumber = 10
        ballNumber = 3
    end if
end subroutine FrameBallNumber

subroutine inputMessage(remainingPins, knockedPins)
    use publicVariables
    integer remainingPins, knockedPins

    print '(A, i3, A, i3, A)',&
            '<<-- Frame', frameNumber, ' * Ball', ballNumber, ' -->>'
    do while (.true.)
        print '(A, i2)', 'Enter between 0 to ', remainingPins
        read *, knockedPins
        if (knockedPins>=0.and.knockedPins<=remainingPins) then
            exit
        else
            print*, 'Wrong input'
        end if
    end do
end subroutine inputMessage

subroutine resetPin
    use publicVariables
    integer knockedPins

    call inputMessage(10, knockedPins)
    scoreBoard(indexing) = knockedPins
    ballThrow(indexing) = .true.
end subroutine resetPin

subroutine throwAgain
    use publicVariables
    integer knockedPins

    call inputMessage(10-scoreBoard(indexing-1), knockedPins)
    scoreBoard(indexing) = knockedPins
    ballThrow(indexing) = .true.
end subroutine throwAgain

subroutine ballNotThrown
    use publicVariables

    scoreBoard(indexing)=0
    ballThrow(indexing) = .false.
end subroutine ballNotThrown

subroutine pinControlAndScoring
    use publicVariables

    call frameBallNumber
    if (frameNumber<10) then
        if (ballNumber/=1.and.scoreBoard(indexing-1)==10) then
            call ballNotThrown
        elseif (ballNumber==2) then
            call throwAgain
        else
            call resetPin
        end if
    else
        if (ballNumber==1) then
            call resetPin
        elseif (ballNumber==2)then
            if (scoreBoard(indexing-1)==10) then
                call resetPin
            else
                call throwAgain
            end if
        else
            if (sum(scoreBoard(indexing-2:indexing-1))<10) then
                call ballNotThrown
            elseif (scoreBoard(indexing-1)==10.or.&
                    sum(scoreBoard(indexing-2:indexing-1))==10) then
                call resetPin
            else
                call throwAgain
            end if
        end if
    end if
    call displayScoreBoard
end subroutine pinControlAndScoring

subroutine frameScoreCalculator(idx, bonusBall)
    use publicVariables
    integer :: bonusBall, idx, bonusScore, additionalBall, step

    bonusScore = 0
    additionalBall = 0
    step = 2

    if (count(ballThrow(idx+2:21).eqv..true.)>=bonusBall) then
        do while (additionalBall<bonusBall)
            if (ballThrow(idx+step)) then
                bonusScore = bonusScore + scoreBoard(idx+step)
                additionalBall = additionalBall+1
            end if
            step = step+1
        end do
        frameScore(idx/2+1) = sum(scoreBoard(idx:idx+1))+bonusScore
        frameScoreCalculated(idx/2+1) = .true.
    end if
end subroutine frameScoreCalculator

subroutine displayScoreBoard
    use publicVariables
    integer number, idx
    character(len=1) :: score1, score2, score3
    character(len=3) :: frame, total
    character(len=73) :: lineOne, lineTwo, lineThree, lineFour

    if ((frameNumber<10.and.ballNumber==2).or.ballNumber==3) then
        print *, ''
        print *, 'SCORE BOARD'
        lineOne = '   Frame    |'
        lineTwo = '   Result   |'
        lineThree = 'Frame Score |'
        lineFour = 'Total Score |'

        do number=1,10
            write(frame, '(i2)') number
            lineOne = trim(lineOne)//' '//frame//' |'
        end do

        do idx=1,indexing,2
            if (idx==19) then
                frameScore(10) = scoreBoard(19)+scoreBoard(20)+scoreBoard(21)
                frameScoreCalculated(10) = .true.
            elseif (scoreBoard(idx)==10.and..not.ballThrow(idx+1)) then
                call frameScoreCalculator(idx, 2)
            elseif (sum(scoreBoard(idx:idx+1))==10) then
                call frameScoreCalculator(idx,1)
            else
                call frameScoreCalculator(idx,0)
            end if

        end do

        do idx=1,frameNumber
            if (idx/=10) then
                if (scoreBoard(2*idx-1)==10) then
                    lineTwo = trim(lineTwo)//'  X  |'
                elseif (sum(scoreBoard(2*idx-1:2*idx))==10) then
                    write(score1,'(i1)')scoreBoard(2*idx-1)
                    lineTwo = trim(lineTwo)//' '//score1//' / |'
                else
                    write(score1,'(i1)')scoreBoard(2*idx-1)
                    write(score2,'(i1)')scoreBoard(2*idx)
                    lineTwo = trim(lineTwo)//' '//score1//' '//score2//' |'
                end if
            else
                write(score1,'(i1)')scoreBoard(19)
                write(score2,'(i1)')scoreBoard(20)
                write(score3,'(i1)')scoreBoard(21)
                if (scoreBoard(19)==10) then
                    lineTwo = trim(lineTwo)//'X'
                    if (scoreBoard(20)==10) then
                        lineTwo = trim(lineTwo)//' X'
                        if (scoreBoard(21)==10) then
                            lineTwo = trim(lineTwo)//' X|'
                        else
                            lineTwo = trim(lineTwo)//' '//score3//'|'
                        end if
                    else
                        lineTwo = trim(lineTwo)//' '//score2
                        if (sum(scoreBoard(20:21))==10) then
                            lineTwo = trim(lineTwo)//' /|'
                        else
                            lineTwo = trim(lineTwo)//' '//score3//'|'
                        end if
                    end if
                else
                    lineTwo = trim(lineTwo)//score1
                    if (sum(scoreBoard(19:20))==10) then
                        lineTwo = trim(lineTwo)//' /'
                        if (scoreBoard(21)==10) then
                            lineTwo = trim(lineTwo)//' X|'
                        else
                            lineTwo = trim(lineTwo)//' '//score3//'|'
                        end if
                    else
                        lineTwo = trim(lineTwo)//' '//score2//'  |'
                    end if
                end if
            end if

            if (frameScoreCalculated(idx)) then
                write(frame,'(i3)')frameScore(idx)
                lineThree = trim(lineThree)//' '//frame//' |'
                write(total, '(i3)')sum(frameScore(1:idx))
                lineFour = trim(lineFour)//' '//total//' |'
            else
                lineThree = trim(lineThree)//'     |'
                lineFour = trim(lineFour)//'     |'
            end if
        end do

        do idx=frameNumber+1,10
            lineTwo = trim(lineTwo)//'     |'
            lineThree = trim(lineThree)//'     |'
            lineFour = trim(lineFour)//'     |'
        end do

        print *, lineOne
        print *, lineTwo
        print *, lineThree
        print *, lineFour
        print *, ''
    end if
end subroutine displayScoreBoard
