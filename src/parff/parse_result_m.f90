module parff_parse_result_m
    use iso_varying_string, only: varying_string, var_str
    use parff_message_m, only: message_t
    use parff_parsed_value_m, only: parsed_value_t
    use parff_parser_interfaces_m, only: parser_i
    use parff_parser_output_m, only: parser_output_t
    use parff_state_m, only: new_state

    implicit none
    private
    public :: parse_result_t, parse_with

    type :: parse_result_t
        private
        logical :: ok_
        class(parsed_value_t), allocatable :: parsed_
        type(varying_string) :: message_
    contains
        private
        procedure, public :: ok
        procedure, public :: parsed
        procedure, public :: message
    end type

    interface parse_with
        module procedure parse_with_c
        module procedure parse_with_s
    end interface
contains
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
            result_%ok_ = .true.
            allocate(result_%parsed_, source = the_results%parsed)
        else
            result_%ok_ = .false.
            result_%message_ = the_results%message%to_string()
        end if
    end function

    pure function ok(self)
        class(parse_result_t), intent(in) :: self
        logical :: ok

        ok = self%ok_
    end function

    function parsed(self)
        class(parse_result_t), intent(in) :: self
        class(parsed_value_t), allocatable :: parsed

        allocate(parsed, source = self%parsed_)
    end function

    pure function message(self)
        class(parse_result_t), intent(in) :: self
        type(varying_string) :: message

        message = self%message_
    end function
end module
