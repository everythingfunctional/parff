module parff_state_m
    use iso_varying_string, only: varying_string
    use parff_position_m, only: position_t

    implicit none
    private
    public :: state_t, new_state

    type :: state_t
        type(varying_string) :: input
        type(position_t) :: position
    end type
contains
    pure function new_state(input)
        type(varying_string), intent(in) :: input
        type(state_t) :: new_state

        new_state%input = input
    end function
end module
