module parff
    use iso_varying_string, only: &
            varying_string, &
            assignment(=), &
            operator(==), &
            operator(//), &
            len, &
            var_str
    use parff_intermediate_parsed_string_m, only: intermediate_parsed_string_t
    use parff_intermediate_repeat_m, only: intermediate_repeat_t
    use parff_parsed_character_m, only: parsed_character_t
    use parff_parsed_integer_m, only: parsed_integer_t
    use parff_parsed_item_m, only: parsed_item_t
    use parff_parsed_items_m, only: parsed_items_t
    use parff_parsed_nothing_m, only: parsed_nothing_t, PARSED_NOTHING
    use parff_parsed_rational_m, only: parsed_rational_t
    use parff_parsed_string_m, only: parsed_string_t
    use parff_parsed_value_m, only: parsed_value_t
    use strff, only: &
            operator(.includes.), &
            first_character, &
            join, &
            to_string, &
            without_first_character, &
            NEWLINE

    implicit none
    private
    public :: &
            intermediate_parsed_string_t, &
            intermediate_repeat_t, &
            message_t, &
            parse_result_t, &
            parsed_character_t, &
            parsed_integer_t, &
            parsed_item_t, &
            parsed_items_t, &
            parsed_nothing_t, &
            parsed_rational_t, &
            parsed_string_t, &
            parsed_value_t, &
            parser_output_t, &
            position_t, &
            state_t, &
            consumed_ok, &
            drop_then, &
            either, &
            empty_error, &
            empty_ok, &
            many, &
            many1, &
            many1_with_separator, &
            many_with_separator, &
            message, &
            new_state, &
            optionally, &
            parse_char, &
            parse_digit, &
            parse_integer, &
            parse_nothing, &
            parse_rational, &
            parse_string, &
            parse_whitespace, &
            parse_with, &
            repeat_, &
            return_, &
            satisfy, &
            sequence, &
            then_drop, &
            with_label

    type :: position_t
        integer :: line
        integer :: column
    end type

    type :: state_t
        type(varying_string) :: input
        type(position_t) :: position
    end type

    type :: message_t
        type(position_t) :: position
        type(varying_string) :: found
        type(varying_string), allocatable :: expected(:)
    contains
        procedure :: to_string => message_to_string
    end type

    type :: parser_output_t
        logical :: empty
        logical :: ok
        type(message_t) :: message
        ! The following are only defined if ok
        class(parsed_value_t), allocatable :: parsed
        type(varying_string) :: remaining
        type(position_t) :: position
    end type

    type :: parse_result_t
        logical :: ok
        class(parsed_value_t), allocatable :: parsed
        type(varying_string) :: message
    end type

    abstract interface
        pure function match_i(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches
        end function

        function parser_i(state_) result(result_)
            import parser_output_t, state_t
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_
        end function

        function then_parser_i(previous, state_) result(result_)
            import parser_output_t, parsed_value_t, state_t
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_
        end function
    end interface

    interface drop_then
        module procedure drop_then_parser
        module procedure drop_then_result
    end interface

    interface parse_string
        module procedure parse_string_c
        module procedure parse_string_s
    end interface

    interface parse_with
        module procedure parse_with_c
        module procedure parse_with_s
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
contains
    function consumed_ok(parsed, remaining, position, message_)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message_
        type(parser_output_t) :: consumed_ok

        consumed_ok%empty = .false.
        consumed_ok%ok = .true.
        allocate(consumed_ok%parsed, source = parsed)
        consumed_ok%remaining = remaining
        consumed_ok%position = position
        consumed_ok%message = message_
    end function

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
                    state(previous%remaining, previous%position))
            if (.not.previous%empty) then
                result_%empty = .false.
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
                            first_result%parsed, &
                            first_result%remaining, &
                            first_result%position, &
                            first_result%message, &
                            second_result%message)
                else
                    if (second_result%ok) then
                        result_ = merge_ok( &
                                second_result%parsed, &
                                second_result%remaining, &
                                second_result%position, &
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

    function empty_error(message_)
        type(message_t), intent(in) :: message_
        type(parser_output_t) :: empty_error

        empty_error%empty = .true.
        empty_error%ok = .false.
        empty_error%message = message_
    end function

    function empty_ok(parsed, remaining, position, message_)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message_
        type(parser_output_t) :: empty_ok

        empty_ok%empty = .true.
        empty_ok%ok = .true.
        allocate(empty_ok%parsed, source = parsed)
        empty_ok%remaining = remaining
        empty_ok%position = position
        empty_ok%message = message_
    end function

    pure function expect(message_, label) result(new_message)
        type(message_t), intent(in) :: message_
        type(varying_string), intent(in) :: label
        type(message_t) :: new_message

        new_message = message(message_%position, message_%found, [label])
    end function

    function many(the_parser, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = many_with_separator(the_parser, parse_nothing, the_state)
    end function

    function many1(the_parser, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = many1_with_separator(the_parser, parse_nothing, the_state)
    end function

    function many1_with_separator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        procedure(parser_i) :: the_separator
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        type(parsed_items_t) :: all
        type(parser_output_t) :: next

        the_result = the_parser(the_state)
        if (the_result%ok) then
            all = parsed_items_t([parsed_item_t(the_result%parsed)])
            do
                next = drop_then(the_separator, the_parser, state(the_result%remaining, the_result%position))
                if (.not.next%ok) exit
                all = parsed_items_t([all%items(), parsed_item_t(next%parsed)])
                the_result = next
            end do
            deallocate(the_result%parsed)
            allocate(the_result%parsed, source = all)
        end if
    end function

    function many_with_separator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        procedure(parser_i) :: the_separator
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        type(parsed_items_t) :: all

        the_result = many1_with_separator(the_parser, the_separator, the_state)
        if (.not.the_result%ok) then
            all = parsed_items_t([parsed_item_t::])
            the_result = empty_ok( &
                    all, &
                    the_state%input, &
                    the_state%position, &
                    message( &
                            the_state%position, &
                            var_str(""), &
                            [varying_string::]))
        end if
    end function

    pure function merge_(message1, message2) result(merged)
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(message_t) :: merged

        merged = message( &
                message1%position, &
                message1%found, &
                [message1%expected, message2%expected])
    end function

    function merge_error(message1, message2) result(result_)
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(parser_output_t) :: result_

        result_ = empty_error(merge_(message1, message2))
    end function

    function merge_ok( &
            parsed, remaining, position, message1, message2) result(result_)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(parser_output_t) :: result_

        result_ = empty_ok( &
                parsed, &
                remaining, &
                position, &
                merge_(message1, message2))
    end function

    pure function message(position, found, expected)
        type(position_t), intent(in) :: position
        type(varying_string), intent(in) :: found
        type(varying_string), intent(in) :: expected(:)
        type(message_t) :: message

        message%position = position
        message%found = found
        allocate(message%expected, source = expected)
    end function

    pure function message_to_string(self) result(string)
        class(message_t), intent(in) :: self
        type(varying_string) :: string

        string = "At line " // to_string(self%position%line) // " and column " // to_string(self%position%column) // NEWLINE &
                // "    found " // self%found // " but expected " // join(self%expected, " or ")
    end function

    pure function new_position()
        type(position_t) :: new_position

        new_position%line = 1
        new_position%column = 1
    end function

    pure function new_state(input)
        type(varying_string), intent(in) :: input
        type(state_t) :: new_state

        new_state = state(input, new_position())
    end function

    pure function next_position(char_, position)
        character(len=1), intent(in) :: char_
        type(position_t), intent(in) :: position
        type(position_t) :: next_position

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE_ = char(10)

        if (char_ == NEWLINE_) then
            next_position%line = position%line + 1
            next_position%column = position%column
        else if (char_ == TAB) then
            next_position%line = position%line
            next_position%column = position%column + 8 - mod(position%column - 1, 8)
        else
            next_position%line = position%line
            next_position%column = position%column + 1
        end if
    end function

    function optionally(parser, the_state) result(the_result)
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
                select type (parsed_string => result_%parsed)
                type is (parsed_string_t)
                    the_string = parsed_string%value_()
                    read(the_string, *) the_number
                    the_value = parsed_integer_t(the_number)
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_value)
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
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
                    end select
                end select
            end if
        end function

        function parse_digits(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string), allocatable :: digits(:)
            integer :: i
            type(parsed_string_t) :: parsed_digits

            result_ = many1(parse_digit, state_)
            if (result_%ok) then
                select type (results => result_%parsed)
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
                deallocate(result_%parsed)
                parsed_digits = parsed_string_t(join(digits, ""))
                allocate(result_%parsed, source = parsed_digits)
            end if
        end function
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
                select type (parsed_string => result_%parsed)
                type is (parsed_string_t)
                    the_string = parsed_string%value_()
                    read(the_string, *) the_number
                    the_value = parsed_rational_t(the_number)
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_value)
                end select
            end if
        end function

        function parse_sign(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_string_t) :: the_string

            result_ = either(parse_plus, parse_minus, state_)
            if (result_%ok) then
                select type (the_character => result_%parsed)
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
                end select
            else
                the_string = parsed_string_t("")
                result_ = empty_ok( &
                    the_string, &
                    state_%input, &
                    state_%position, &
                    message(state_%position, var_str(""), [varying_string::]))
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
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
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
            type(parsed_string_t) :: parsed_digits

            result_ = many1(parse_digit, state_)
            if (result_%ok) then
                select type (results => result_%parsed)
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
                    deallocate(result_%parsed)
                    parsed_digits = parsed_string_t(join(digits, ""))
                    allocate(result_%parsed, source = parsed_digits)
                end select
            end if
        end function

        function then_parse_fraction(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = sequence(parse_decimal, then_parse_maybe_digits, state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
                    end select
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        message(state_%position, var_str(""), [varying_string::]))
            end if
        end function

        function parse_decimal(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_string_t) :: the_string

            result_ = parse_char(".", state_)
            if (result_%ok) then
                select type (the_character => result_%parsed)
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
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
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
                    end select
                end select
            end if
        end function

        function parse_maybe_digits(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(varying_string), allocatable :: digits(:)
            integer :: i
            type(parsed_string_t) :: parsed_digits

            result_ = many(parse_digit, state_)
            select type (results => result_%parsed)
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
                deallocate(result_%parsed)
                parsed_digits = parsed_string_t(join(digits, ""))
                allocate(result_%parsed, source = parsed_digits)
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
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
                    end select
                end select
            end if
        end function

        function then_parse_exponent(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_exponent(state_)
            if (result_%ok) then
                select type (previous)
                type is (parsed_string_t)
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
                    end select
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        message(state_%position, var_str(""), [varying_string::]))
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
                select type (the_character => result_%parsed)
                type is (parsed_character_t)
                    the_string = parsed_string_t(the_character%value_())
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
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
                    select type (next => result_%parsed)
                    type is (parsed_string_t)
                        next = parsed_string_t(previous%value_() // next%value_())
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

            type(parsed_string_t) :: empty
            type(intermediate_parsed_string_t) :: initial

            if (string == "") then
                empty = parsed_string_t("")
                result_ = empty_ok(empty, state_%input, state_%position, message( &
                        state_%position, var_str(""), [varying_string::]))
            else
                initial = intermediate_parsed_string_t("", string)
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function

        recursive function recurse(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_string_t) :: final_string

            select type (previous)
            type is (intermediate_parsed_string_t)
                if (len(previous%left_to_parse()) == 0) then
                    final_string = parsed_string_t(previous%parsed_so_far())
                    result_ = consumed_ok( &
                            final_string, &
                            state_%input, &
                            state_%position, &
                            message(state_%position, var_str(""), [varying_string::]))
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

            result_ = parse_char(first_character(previous%left_to_parse()), state_)
            if (result_%ok) then
                select type (the_char => result_%parsed)
                type is (parsed_character_t)
                    next = intermediate_parsed_string_t( &
                            previous%parsed_so_far() // the_char%value_(), &
                            without_first_character(previous%left_to_parse()))
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = next)
                end select
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

    function parse_with_c(parser, string) result(result_)
        procedure(parser_i) :: parser
        character(len=*), intent(in) :: string
        type(parse_result_t) :: result_

        result_ = parse_with(parser, var_str(string))
    end function

    function parse_with_s(parser, string) result(result_)
        procedure(parser_i) :: parser
        type(varying_string), intent(in) :: string
        type(parse_result_t) :: result_

        type(parser_output_t) :: the_results

        the_results = parser(new_state(string))
        if (the_results%ok) then
            result_%ok = .true.
            allocate(result_%parsed, source = the_results%parsed)
        else
            result_%ok = .false.
            result_%message = the_results%message%to_string()
        end if
    end function

    function repeat_(the_parser, times, the_state) result(the_result)
        procedure(parser_i) :: the_parser
        integer, intent(in) :: times
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = start(the_state)
    contains
        function start(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_items_t) :: empty
            type(intermediate_repeat_t) :: initial

            if (times <= 0) then
                empty = parsed_items_t([parsed_item_t::])
                result_ = empty_ok(empty, state_%input, state_%position, message( &
                        state_%position, var_str(""), [varying_string::]))
            else
                initial%remaining = times
                initial%parsed_so_far = parsed_items_t([parsed_item_t::])
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function

        recursive function recurse(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(parsed_items_t) :: final_list

            select type (previous)
            type is (intermediate_repeat_t)
                if (previous%remaining <= 0) then
                    final_list = previous%parsed_so_far
                    result_ = consumed_ok( &
                            final_list, &
                            state_%input, &
                            state_%position, &
                            message(state_%position, var_str(""), [varying_string::]))
                else
                    result_ = sequence(parse_next(previous, state_), recurse)
                end if
            end select
        end function

        function parse_next(previous, state_) result(result_)
            type(intermediate_repeat_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            type(intermediate_repeat_t) :: next
            type(parsed_item_t) :: this_item

            result_ = the_parser(state_)
            if (result_%ok) then
                next%remaining = previous%remaining - 1
                this_item = parsed_item_t(result_%parsed)
                next%parsed_so_far = parsed_items_t([previous%parsed_so_far%items(), this_item])
                deallocate(result_%parsed)
                allocate(result_%parsed, source = next)
            end if
        end function
    end function

    function return_(parsed, state_) result(result_)
        class(parsed_value_t), intent(in) :: parsed
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = empty_ok( &
                parsed, state_%input, state_%position, message( &
                        state_%position, var_str(""), [varying_string::]))
    end function

    function satisfy(matches, state_) result(result_)
        procedure(match_i) :: matches
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        character(len=1) :: first_character_
        type(position_t) :: new_position
        type(parsed_character_t) :: parsed_character

        if (len(state_%input) > 0) then
            first_character_ = first_character(state_%input)
            if (matches(first_character_)) then
                new_position = next_position(first_character_, state_%position)
                parsed_character = parsed_character_t(first_character_)
                result_ = consumed_ok( &
                        parsed_character, &
                        without_first_character(state_%input), &
                        new_position, &
                        message( &
                                new_position, &
                                var_str(""), &
                                [varying_string::]))
            else
                result_ = empty_error(message( &
                        state_%position, &
                        var_str(first_character_), &
                        [varying_string::]))
            end if
        else
            result_ = empty_error(message( &
                    state_%position, &
                    var_str("end of input"), &
                    [varying_string::]))
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
                    previous%parsed, &
                    state(previous%remaining, previous%position))
            if (.not.previous%empty) then
                result_%empty = .false.
            end if
        else
            result_ = previous
        end if
    end function

    pure function state(input, position)
        type(varying_string), intent(in) :: input
        type(position_t), intent(in) :: position
        type(state_t) :: state

        state%input = input
        state%position = position
    end function

    function then_drop_parser(parser1, parser2, state_) result(result_)
        procedure(parser_i) :: parser1
        procedure(parser_i) :: parser2
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = then_drop(parser1(state_), parser2)
    end function

    function then_drop_result(previous, parser) result(result_)
        type(parser_output_t), intent(in) :: previous
        procedure(parser_i) :: parser
        type(parser_output_t) :: result_

        if (previous%ok) then
            result_ = parser( &
                    state(previous%remaining, previous%position))
            result_%empty = previous%empty .and. result_%empty
            if (result_%ok) then
                deallocate(result_%parsed)
                allocate(result_%parsed, source = previous%parsed)
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
            if (the_result%ok) then
                the_message = expect(the_result%message, label)
                result_ = empty_ok( &
                        the_result%parsed, &
                        the_result%remaining, &
                        the_result%position, &
                        the_message)
            else
                the_message = expect(the_result%message, label)
                result_ = empty_error(the_message)
            end if
        else
            result_ = the_result
        end if
    end function
end module
