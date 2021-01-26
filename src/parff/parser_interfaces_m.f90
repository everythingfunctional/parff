module parff_parser_interfaces_m
    use parff_parsed_value_m, only: parsed_value_t
    use parff_parser_output_m, only: parser_output_t
    use parff_state_m, only: state_t

    implicit none
    private
    public :: match_i, parser_i, then_parser_i

    abstract interface
        pure function match_i(char_) result(matches)
            implicit none

            character(len=1), intent(in) :: char_
            logical :: matches
        end function

        function parser_i(state_) result(result_)
            import :: parser_output_t, state_t

            implicit none

            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_
        end function

        function then_parser_i(previous, state_) result(result_)
            import :: parser_output_t, parsed_value_t, state_t

            implicit none

            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_
        end function
    end interface
end module
