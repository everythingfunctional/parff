module parff_parsers_m
    use iso_varying_string, only: &
            varying_string, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            var_str
    use parff_intermediate_parsed_string_m, only: intermediate_parsed_string_t
    use parff_intermediate_repeat_m, only: intermediate_repeat_t
    use parff_message_m, only: message_t, expect
    use parff_parsed_character_m, only: parsed_character_t
    use parff_parsed_integer_m, only: parsed_integer_t
    use parff_parsed_item_m, only: parsed_item_t
    use parff_parsed_items_m, only: parsed_items_t
    use parff_parsed_nothing_m, only: PARSED_NOTHING
    use parff_parsed_rational_m, only: parsed_rational_t
    use parff_parsed_string_m, only: parsed_string_t
    use parff_parsed_value_m, only: parsed_value_t
    use parff_parser_interfaces_m, only: match_i, parser_i, then_parser_i
    use parff_parser_output_m, only: &
            parser_output_t, &
            consumed_ok, &
            empty_ok, &
            empty_error, &
            merge_ok, &
            merge_error
    use parff_position_m, only: position_t
    use parff_state_m, only: state_t
    use strff, only: &
            operator(.includes.), &
            first_character, &
            join, &
            without_first_character, &
            NEWLINE

    implicit none
    private
    public :: &
            drop_then, &
            either, &
            many, &
            many1, &
            many1_with_separator, &
            many_with_separator, &
            optionally, &
            parse_char, &
            parse_digit, &
            parse_end_of_input, &
            parse_integer, &
            parse_nothing, &
            parse_rational, &
            parse_string, &
            parse_whitespace, &
            repeat_, &
            return_, &
            satisfy, &
            sequence, &
            then_drop, &
            with_label

    interface drop_then
        module procedure drop_then_parser
        module procedure drop_then_result
    end interface

    interface parse_string
        module procedure parse_string_c
        module procedure parse_string_s
    end interface

    interface sequence
        module procedure sequence_parser
        module procedure sequence_result
    end interface

    interface then_drop
        module procedure then_drop_parser
        module procedure then_drop_result
    end interface

    interface with_label
        module procedure with_label_c
        module procedure with_label_s
    end interface

    character(len=*), parameter :: END_OF_INPUT = "end of input"
contains
    recursive function drop_then_parser(parser1, parser2, state_) result(result_)
        procedure(parser_i) :: parser1
        procedure(parser_i) :: parser2
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = drop_then(parser1(state_), parser2)
    end function

    recursive function drop_then_result(previous, parser) result(result_)
        type(parser_output_t), intent(in) :: previous
        procedure(parser_i) :: parser
        type(parser_output_t) :: result_

        if (previous%ok) then
            result_ = parser( &
                    state_t(previous%remaining(), previous%position()))
            if (.not.previous%empty) then
                result_ = result_%but_not_empty()
            end if
        else
            result_ = previous
        end if
    end function

    recursive function either(parse1, parse2, state_) result(result_)
        procedure(parser_i) :: parse1
        procedure(parser_i) :: parse2
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(parser_output_t) :: first_result
        type(parser_output_t) :: second_result

        first_result = parse1(state_)

        if (first_result%empty) then
            second_result = parse2(state_)
            if (second_result%empty) then
                if (first_result%ok) then
                    result_ = merge_ok( &
                            first_result%parsed(), &
                            first_result%remaining(), &
                            first_result%position(), &
                            first_result%message, &
                            second_result%message)
                else
                    if (second_result%ok) then
                        result_ = merge_ok( &
                                second_result%parsed(), &
                                second_result%remaining(), &
                                second_result%position(), &
                                first_result%message, &
                                second_result%message)
                    else
                        result_ = merge_error( &
                                first_result%message, &
                                second_result%message)
                    end if
                end if
            else
                result_ = second_result
            end if
        else
            result_ = first_result
        end if
    end function

    recursive function many(the_parser, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = many_with_separator(the_parser, parse_nothing, the_state)
    end function

    recursive function many1(the_parser, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = many1_with_separator(the_parser, parse_nothing, the_state)
    end function

    recursive function many1_with_separator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        procedure(parser_i) :: the_separator
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        type(parsed_items_t) :: all
        type(parser_output_t) :: next

        the_result = the_parser(the_state)
        if (the_result%ok) then
            all = parsed_items_t([parsed_item_t(the_result%parsed())])
            do
                next = drop_then(the_separator, the_parser, state_t(the_result%remaining(), the_result%position()))
                if (.not.next%ok) exit
                all = parsed_items_t([all%items(), parsed_item_t(next%parsed())])
                the_result = next
            end do
            the_result = the_result%with_parsed_value(all)
        end if
    end function

    recursive function many_with_separator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        procedure(parser_i) :: the_separator
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        type(parsed_item_t) :: EMPTY_PARSED_ITEM(0)
        type(varying_string) :: EMPTY_VARYING_STRING(0)
        type(parsed_items_t) :: all

        the_result = many1_with_separator(the_parser, the_separator, the_state)
        if (.not.the_result%ok) then
            all = parsed_items_t(EMPTY_PARSED_ITEM)
            the_result = empty_ok( &
                    all, &
                    the_state%input, &
                    the_state%position, &
                    message_t( &
                            the_state%position, &
                            var_str(""), &
                            EMPTY_VARYING_STRING))
        end if
    end function

    recursive function optionally(parser, the_state) result(the_result)
        procedure(parser_i) :: parser
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = either(parser, parse_nothing, the_state)
    end function

    function parse_char(the_char, the_state) result(the_result)
        character(len=1), intent(in) :: the_char
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label(the_char, the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
        end function

        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = char_ == the_char
        end function
    end function

    function parse_digit(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("digit", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
        end function

        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = "0123456789".includes.char_
        end function
    end function

    function parse_integer(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("integer", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            integer :: the_number
            character(len=64) :: the_string
            type(parsed_integer_t) :: the_value

            result_ = sequence(optionally(parse_sign, state_), then_parse_digits)
            if (result_%ok) then
                select type (parsed_string => result_%parsed())
                type is (parsed_string_t)
                    the_string = parsed_string%value_()
                    read(the_string, *) the_number
                    the_value = parsed_integer_t(the_number)
                    result_ = result_%with_parsed_value(the_value)
                end select
            end if
        end function

        function parse_sign(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_plus, parse_minus, state_)
        end function

        function parse_plus(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("+", state_)
        end function

        function parse_minus(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("-", state_)
        end function

        function then_parse_digits(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_digits(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_character_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            end if
        end function

        function parse_digits(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string), allocatable :: digits(:)
            integer :: i

            result_ = many1(parse_digit, state_)
            if (result_%ok) then
                select type (results => result_%parsed())
                type is (parsed_items_t)
                    associate(items => results%items())
                        allocate(digits(size(items)))
                        do i = 1, size(digits)
                            select type (string => items(i)%item())
                            type is (parsed_character_t)
                                digits(i) = string%value_()
                            end select
                        end do
                    end associate
                end select
                result_ = result_%with_parsed_value(parsed_string_t(join(digits, "")))
            end if
        end function
    end function

    function parse_end_of_input(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        if (len(the_state%input) > 0) then
            the_result = empty_error(message_t( &
                    the_state%position, &
                    var_str(first_character(the_state%input)), &
                    [var_str(END_OF_INPUT)]))
        else
            the_result = empty_ok( &
                    PARSED_NOTHING, &
                    the_state%input, &
                    the_state%position, &
                    message_t( &
                            the_state%position, &
                            var_str(END_OF_INPUT), &
                            [var_str(END_OF_INPUT)]))
        end if
    end function

    function parse_nothing(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = return_(PARSED_NOTHING, the_state)
    end function

    function parse_rational(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("rational", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            double precision :: the_number
            character(len=64) :: the_string
            type(parsed_rational_t) :: the_value

            result_ = sequence( &
                    sequence(parse_sign, then_parse_number, state_), &
                    then_parse_exponent)
            if (result_%ok) then
                select type (parsed_string => result_%parsed())
                type is (parsed_string_t)
                    the_string = parsed_string%value_()
                    read(the_string, *) the_number
                    the_value = parsed_rational_t(the_number)
                    result_ = result_%with_parsed_value(the_value)
                end select
            end if
        end function

        function parse_sign(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)
            type(parsed_string_t) :: the_string

            result_ = either(parse_plus, parse_minus, state_)
            if (result_%ok) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    result_ = result_%with_parsed_value(the_string)
                end select
            else
                the_string = parsed_string_t("")
                result_ = empty_ok( &
                    the_string, &
                    state_%input, &
                    state_%position, &
                    message_t(state_%position, var_str(""), EMPTY_VARYING_STRING))
            end if
        end function

        function parse_plus(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("+", state_)
        end function

        function parse_minus(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("-", state_)
        end function

        function then_parse_number(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_covered_decimal, parse_uncovered_decimal, state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            end if
        end function

        function parse_covered_decimal(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = sequence(parse_digits, then_parse_fraction, state_)
        end function

        function parse_digits(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string), allocatable :: digits(:)
            integer :: i

            result_ = many1(parse_digit, state_)
            if (result_%ok) then
                select type (results => result_%parsed())
                type is (parsed_items_t)
                    associate(items => results%items())
                        allocate(digits(size(items)))
                        do i = 1, size(digits)
                            select type (string => items(i)%item())
                            type is (parsed_character_t)
                                digits(i) = string%value_()
                            end select
                        end do
                    end associate
                    result_ = result_%with_parsed_value( &
                            parsed_string_t(join(digits, "")))
                end select
            end if
        end function

        function then_parse_fraction(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)

            result_ = sequence(parse_decimal, then_parse_maybe_digits, state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        message_t(state_%position, var_str(""), EMPTY_VARYING_STRING))
            end if
        end function

        function parse_decimal(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_string_t) :: the_string

            result_ = parse_char(".", state_)
            if (result_%ok) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    result_ = result_%with_parsed_value(the_string)
                end select
            end if
        end function

        function then_parse_maybe_digits(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_maybe_digits(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            end if
        end function

        function parse_maybe_digits(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string), allocatable :: digits(:)
            integer :: i

            result_ = many(parse_digit, state_)
            select type (results => result_%parsed())
            type is (parsed_items_t)
                associate(items => results%items())
                    allocate(digits(size(items)))
                    do i = 1, size(digits)
                        select type (string => items(i)%item())
                        type is (parsed_character_t)
                            digits(i) = string%value_()
                        end select
                    end do
                end associate
                result_ = result_%with_parsed_value(parsed_string_t(join(digits, "")))
            end select
        end function

        function parse_uncovered_decimal(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = sequence(parse_decimal, then_parse_digits, state_)
        end function

        function then_parse_digits(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_digits(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            end if
        end function

        function then_parse_exponent(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)

            result_ = parse_exponent(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        message_t(state_%position, var_str(""), EMPTY_VARYING_STRING))
            end if
        end function

        function parse_exponent(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = sequence( &
                    sequence(parse_letter, then_parse_sign, state_), &
                    then_parse_digits)
        end function

        function parse_letter(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_string_t) :: the_string

            result_ = either(parse_e, parse_d, state_)
            if (result_%ok) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    result_ = result_%with_parsed_value(the_string)
                end select
            end if
        end function

        function parse_e(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_upper_e, parse_lower_e, state_)
        end function

        function parse_upper_e(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("E", state_)
        end function

        function parse_lower_e(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("e", state_)
        end function

        function parse_d(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_upper_d, parse_lower_d, state_)
        end function

        function parse_upper_d(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("D", state_)
        end function

        function parse_lower_d(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("d", state_)
        end function

        function then_parse_sign(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_sign(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed())
                    type is (parsed_string_t)
                        result_ = result_%with_parsed_value( &
                                parsed_string_t(previous%value_() // next%value_()))
                    end select
                end select
            end if
        end function
    end function

    function parse_string_c(string, the_state) result(the_result)
        character(len=*), intent(in) :: string
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = parse_string(var_str(string), the_state)
    end function

    function parse_string_s(string, the_state) result(the_result)
        type(varying_string), intent(in) :: string
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label(string, start, the_state)
    contains
        function start(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)
            type(parsed_string_t) :: empty
            type(intermediate_parsed_string_t) :: initial

            if (string == "") then
                empty = parsed_string_t("")
                result_ = empty_ok(empty, state_%input, state_%position, message_t( &
                        state_%position, var_str(""), EMPTY_VARYING_STRING))
            else
                initial = intermediate_parsed_string_t("", string)
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function

        recursive function recurse(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)
            type(parsed_string_t) :: final_string

            select type (previous)
            type is (intermediate_parsed_string_t)
                if (len(previous%left_to_parse()) == 0) then
                    final_string = parsed_string_t(previous%parsed_so_far())
                    result_ = consumed_ok( &
                            final_string, &
                            state_%input, &
                            state_%position, &
                            message_t(state_%position, var_str(""), EMPTY_VARYING_STRING))
                else
                    result_ = sequence(parse_next(previous, state_), recurse)
                end if
            end select
        end function

        function parse_next(previous, state_) result(result_)
            type(intermediate_parsed_string_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(intermediate_parsed_string_t) :: next
            type(varying_string) :: was_parsed

            result_ = parse_char(first_character(previous%left_to_parse()), state_)
            if (result_%ok) then
                select type (the_char => result_%parsed())
                type is (parsed_character_t)
                    next = intermediate_parsed_string_t( &
                            previous%parsed_so_far() // the_char%value_(), &
                            without_first_character(previous%left_to_parse()))
                    result_ = result_%with_parsed_value(next)
                end select
            else
                if (len(state_%input) == 0) then
                    if (len(previous%parsed_so_far()) == 0) then
                        was_parsed = "<nothing>"
                    else
                        was_parsed = previous%parsed_so_far()
                    end if
                else
                    if (len(previous%parsed_so_far()) == 0) then
                        was_parsed = first_character(state_%input)
                    else
                        was_parsed = previous%parsed_so_far() // first_character(state_%input)
                    end if
                end if
                result_ = empty_error(message_t( &
                        state_%position, &
                        was_parsed, &
                        [string]))
            end if
        end function
    end function

    function parse_whitespace(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("whitespace", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
        end function

        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=1), parameter :: TAB = char(9)
            character(len=1), parameter :: CARRIAGE_RETURN = char(13)
            character(len=1), parameter :: SPACE = char(32)
            character(len=*), parameter :: WHITESPACE = &
                    TAB // NEWLINE // CARRIAGE_RETURN // SPACE

            matches = WHITESPACE.includes.char_
        end function
    end function

    recursive function repeat_(the_parser, times, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        integer, intent(in) :: times
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = start(the_state)
    contains
        recursive function start(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)
            type(parsed_item_t) :: EMPTY_PARSED_ITEM(0)
            type(parsed_items_t) :: empty
            type(intermediate_repeat_t) :: initial

            if (times <= 0) then
                empty = parsed_items_t(EMPTY_PARSED_ITEM)
                result_ = empty_ok(empty, state_%input, state_%position, message_t( &
                        state_%position, var_str(""), EMPTY_VARYING_STRING))
            else
                initial = intermediate_repeat_t(parsed_items_t(EMPTY_PARSED_ITEM), times)
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function

        recursive function recurse(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string) :: EMPTY_VARYING_STRING(0)

            select type (previous)
            type is (intermediate_repeat_t)
                if (previous%remaining() <= 0) then
                    result_ = consumed_ok( &
                            previous%parsed_so_far(), &
                            state_%input, &
                            state_%position, &
                            message_t(state_%position, var_str(""), EMPTY_VARYING_STRING))
                else
                    result_ = sequence(parse_next(previous, state_), recurse)
                end if
            end select
        end function

        recursive function parse_next(previous, state_) result(result_)
            type(intermediate_repeat_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(intermediate_repeat_t) :: next
            type(parsed_items_t) :: parsed_so_far
            type(parsed_item_t) :: this_item

            result_ = the_parser(state_)
            if (result_%ok) then
                this_item = parsed_item_t(result_%parsed())
                parsed_so_far = previous%parsed_so_far()
                next = intermediate_repeat_t( &
                        parsed_items_t([parsed_so_far%items(), this_item]), &
                        previous%remaining() - 1)
                result_ = result_%with_parsed_value(next)
            end if
        end function
    end function

    function return_(parsed, state_) result(result_)
        class(parsed_value_t), intent(in) :: parsed
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string) :: EMPTY_VARYING_STRING(0)

        result_ = empty_ok( &
                parsed, state_%input, state_%position, message_t( &
                        state_%position, var_str(""), EMPTY_VARYING_STRING))
    end function

    function satisfy(matches, state_) result(result_)
        procedure(match_i) :: matches
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string) :: EMPTY_VARYING_STRING(0)

        character(len=1) :: first_character_
        type(position_t) :: new_position
        type(parsed_character_t) :: parsed_character

        if (len(state_%input) > 0) then
            first_character_ = first_character(state_%input)
            if (matches(first_character_)) then
                new_position = state_%position%next_position(first_character_)
                parsed_character = parsed_character_t(first_character_)
                result_ = consumed_ok( &
                        parsed_character, &
                        without_first_character(state_%input), &
                        new_position, &
                        message_t( &
                                new_position, &
                                var_str(""), &
                                EMPTY_VARYING_STRING))
            else
                result_ = empty_error(message_t( &
                        state_%position, &
                        var_str(first_character_), &
                        EMPTY_VARYING_STRING))
            end if
        else
            result_ = empty_error(message_t( &
                    state_%position, &
                    var_str(END_OF_INPUT), &
                    EMPTY_VARYING_STRING))
        end if
    end function

    recursive function sequence_parser(parser1, parser2, state_) result(result_)
        procedure(parser_i) :: parser1
        procedure(then_parser_i) :: parser2
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = sequence(parser1(state_), parser2)
    end function

    recursive function sequence_result(previous, parser) result(result_)
        type(parser_output_t), intent(in) :: previous
        procedure(then_parser_i) :: parser
        type(parser_output_t) :: result_

        if (previous%ok) then
            result_ = parser( &
                    previous%parsed(), &
                    state_t(previous%remaining(), previous%position()))
            if (.not.previous%empty.and.result_%ok) then
                result_ = result_%but_not_empty()
            end if
        else
            result_ = previous
        end if
    end function

    recursive function then_drop_parser(parser1, parser2, state_) result(result_)
        procedure(parser_i) :: parser1
        procedure(parser_i) :: parser2
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = then_drop(parser1(state_), parser2)
    end function

    recursive function then_drop_result(previous, parser) result(result_)
        type(parser_output_t), intent(in) :: previous
        procedure(parser_i) :: parser
        type(parser_output_t) :: result_

        if (previous%ok) then
            result_ = parser( &
                    state_t(previous%remaining(), previous%position()))
            if (.not. previous%empty) then
                result_ = result_%but_not_empty()
            end if
            if (result_%ok) then
                result_ = result_%with_parsed_value(previous%parsed())
            end if
        else
            result_ = previous
        end if
    end function

    recursive function with_label_c(label, parse, state_) result(result_)
        character(len=*), intent(in) :: label
        procedure(parser_i) :: parse
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = with_label(var_str(label), parse, state_)
    end function

    recursive function with_label_s(label, parse, state_) result(result_)
        type(varying_string), intent(in) :: label
        procedure(parser_i) :: parse
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(parser_output_t) :: the_result
        type(message_t) :: the_message

        the_result = parse(state_)
        if (the_result%empty) then
            the_message = expect(the_result%message, label)
            if (the_result%ok) then
                result_ = empty_ok( &
                        the_result%parsed(), &
                        the_result%remaining(), &
                        the_result%position(), &
                        the_message)
            else
                result_ = empty_error(the_message)
            end if
        else
            result_ = the_result
        end if
    end function
end module
