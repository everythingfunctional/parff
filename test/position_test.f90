module position_test
    use iso_varying_string, only: var_str
    use parff, only: &
            many, &
            new_state, &
            parser_output_t, &
            satisfy, &
            state_t
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            describe, &
            it

    implicit none
    private

    public :: test_position
contains
    function test_position() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "position_t", &
                [ it( &
                        "normal characters only increment the column", &
                        check_simple_char) &
                , it( &
                        "tab characters increment the column to the next multiple of 8", &
                        check_tab) &
                , it( &
                        "newlines increment the line and reset the column", &
                        check_newline) &
                ])
    end function

    function check_simple_char() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_everything(new_state(var_str("A")))

        associate(position => parse_result%position())
            result_ = assert_equals(1, position%line(), &
                            var_str("don't change line")) &
                    .and. &
                    assert_equals(2, position%column(), &
                            var_str("increment column"))
        end associate
    end function

    function check_tab() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_everything( &
                new_state(var_str("A" // char(9))))

        associate(position => parse_result%position())
            result_ = assert_equals(1, position%line(), &
                            var_str("don't change line")) &
                    .and. &
                    assert_equals(9, position%column(), &
                            var_str("increment column"))
        end associate
    end function

    function check_newline() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_everything( &
                new_state(var_str('A' // new_line('A'))))

        associate(position => parse_result%position())
            result_ = assert_equals(2, position%line(), &
                                var_str("increment line")) &
                        .and. &
                        assert_equals(1, position%column(), &
                                var_str("reset column"))
        end associate
    end function

    function parse_everything(the_state) result(the_result)
        type(state_t) :: the_state
        type(parser_output_t) :: the_result

        the_result = many(parse_anything, the_state)
    contains
        function parse_anything(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(match_everything, state_)
        end function

        pure function match_everything(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            ! suppress 'unused dummy argument' warning
            matches = char_.eq.char_
        end function
    end function
end module
