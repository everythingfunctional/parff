program test
    implicit none

    call run()
contains
    subroutine run()
        use drop_then_test, only: &
            drop_then_drop_then => test_drop_then
        use either_parse_test, only: &
            either_parse_or_parse => test_or_parse
        use many1_test, only: &
            many1_many1 => test_many1
        use many1_with_separator_test, only: &
            many1_with_separator_many1_with_separator => test_many1_with_separator
        use many_test, only: &
            many_many => test_many
        use many_with_separator_test, only: &
            many_with_separator_many_with_separator => test_many_with_separator
        use optionally_test, only: &
            optionally_optionally => test_optionally
        use parse_character_test, only: &
            parse_character_parse_character => test_parse_character
        use parse_digit_test, only: &
            parse_digit_parse_digit => test_parse_digit
        use parse_string_test, only: &
            parse_string_parse_string => test_parse_string
        use parse_whitespace_test, only: &
            parse_whitespace_parse_whitespace => test_parse_whitespace
        use parse_with_test, only: &
            parse_with_parse_with => test_parse_with
        use repeat_test, only: &
            repeat_repeat => test_repeat
        use sequence_test, only: &
            sequence_sequence => test_sequence
        use then_drop_test, only: &
            then_drop_then_drop => test_then_drop
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(15)

        individual_tests(1) = drop_then_drop_then()
        individual_tests(2) = either_parse_or_parse()
        individual_tests(3) = many1_many1()
        individual_tests(4) = many1_with_separator_many1_with_separator()
        individual_tests(5) = many_many()
        individual_tests(6) = many_with_separator_many_with_separator()
        individual_tests(7) = optionally_optionally()
        individual_tests(8) = parse_character_parse_character()
        individual_tests(9) = parse_digit_parse_digit()
        individual_tests(10) = parse_string_parse_string()
        individual_tests(11) = parse_whitespace_parse_whitespace()
        individual_tests(12) = parse_with_parse_with()
        individual_tests(13) = repeat_repeat()
        individual_tests(14) = sequence_sequence()
        individual_tests(15) = then_drop_then_drop()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program test
