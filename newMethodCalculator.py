"""Bowling Score Calculator in New Method.

Only the python standard libraries are used.
Implementing 2-D list.
"""
from time import process_time
from typing import Optional


def get_info(frame_id: int, ball_id: int, pins: int = 10) -> int:
    user_input = input(f'<<-- Frame {frame_id + 1} * Ball {ball_id + 1} -->>\n'
                       f'Enter between 0 to {pins}: ')
    while True:
        try:
            int(user_input)
        except ValueError:
            user_input = input(f'Enter between 0 to {pins}: ')
        else:
            if 0 <= int(user_input) <= pins:
                return int(user_input)
            else:
                user_input = f'Input out of range.'
                print(user_input, end=' ')


def calculate_frame_score(board: list, current_frame: int,
                          bonus_count: int = 0) -> Optional[int]:
    score = sum(filter(None, board[current_frame]))
    bonus = [pin for frame in board[current_frame + 1:current_frame + 3]
             for pin in frame if pin is not None]
    return score + sum(bonus[:bonus_count]) if len(bonus) >= bonus_count \
        else None


def display_board(board: list) -> None:
    msg = [f'\nSCORE BOARD', f'{"Frame":^12s}|', f'{"Result":^12s}|',
           f'{"Frame Score":^12s}|', f'{"Total Score":^12s}|']
    total = 0
    for frame_id, pins in enumerate(board):
        msg[1] += f'{frame_id + 1:^5d}|'
        if pins == [10, None]:
            msg[2] += f'  X  |'
            frame = calculate_frame_score(board, frame_id, 2)
        elif sum(filter(None, pins)) == 10 and frame_id != 9:
            msg[2] += f' {pins[0]} / |'
            frame = calculate_frame_score(board, frame_id, 1)
        elif None not in pins:
            if frame_id != 9:
                msg[2] += f' {pins[0]} {pins[1]} |'
                frame = calculate_frame_score(board, frame_id)
            else:
                if len(pins) == 2:
                    msg[2] += f' {pins[0]} {pins[1]} |'
                else:
                    if pins[0] == 10:
                        msg[2] += 'X '
                        if pins[1] == 10:
                            msg[2] += 'X '
                            msg[2] += 'X|' if pins[2] == 10 else f'{pins[2]}|'
                        else:
                            msg[2] += f'{pins[1]} /|' if sum(pins[1:]) == 10 \
                                else f'{pins[1]} {pins[2]}|'
                    else:
                        msg[2] += f'{pins[0]} / '
                        msg[2] += 'X|' if pins[2] == 10 else f'{pins[2]}|'
                frame = sum(pins)
        else:
            msg[2] += f'     |'
            frame = None

        total = None if frame is None else total + frame

        for _, item in enumerate([frame, total]):
            msg[3 + _] += (lambda x:
                           f'{x:^5d}|' if x is not None else f'     |')(item)

    print(*msg, sep='\n', end='\n\n')
    return


def normal_frame(board: list, frame_id: int) -> list:
    board[frame_id][0] = get_info(frame_id, 0)
    if board[frame_id][0] != 10:
        board[frame_id][1] = get_info(frame_id, 1, 10 - board[frame_id][0])
    display_board(board)
    return board


def last_frame(board: list) -> list:
    board[-1][0] = get_info(9, 0)
    if board[-1][0] == 10:
        board[-1][1] = get_info(9, 1)
        if board[-1][1] == 10:
            board[-1].append(get_info(9, 2))
        else:
            board[-1].append(get_info(9, 2, 10 - board[-1][1]))
    else:
        board[-1][1] = get_info(9, 1, 10 - board[-1][0])
        if sum(board[-1][:2]) == 10:
            board[-1].append(get_info(9, 2))
    display_board(board)
    return board


start = process_time()
rawBoard = [[None, None] for _ in range(10)]
print('<<--    GAME START    -->>')
for frameID in range(9):
    rawBoard = normal_frame(board=rawBoard, frame_id=frameID)
rawBoard = last_frame(board=rawBoard)
print('<<--    END OF GAME   -->>')
print(f'Elapsed time: {(process_time() - start) * 1000:.5f}ms.')
