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
        logical :: ok
        class(parsed_value_t), allocatable :: parsed
        type(varying_string) :: message
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
            result_%ok = .true.
            allocate(result_%parsed, source = the_results%parsed)
        else
            result_%ok = .false.
            result_%message = the_results%message%to_string()
        end if
    end function
end module
