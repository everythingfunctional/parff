module parff_state_m
    use iso_varying_string, only: varying_string
    use parff_position_m, only: position_t

    implicit none
    private
    public :: state_t, new_state

    type :: state_t
        private
        type(varying_string) :: input_
        type(position_t) :: position_
    contains
        private
        procedure, public :: input
        procedure, public :: position
    end type

    interface state_t
        module procedure constructor
    end interface
contains
    pure function new_state(input)
        type(varying_string), intent(in) :: input
        type(state_t) :: new_state

        new_state%input_ = input
    end function

    pure function constructor(input, position) result(state)
        type(varying_string), intent(in) :: input
        type(position_t), intent(in) :: position
        type(state_t) :: state

        state%input_ = input
        state%position_ = position
    end function

    pure function input(self)
        class(state_t), intent(in) :: self
        type(varying_string) :: input

        input = self%input_
    end function

    pure function position(self)
        class(state_t), intent(in) :: self
        type(position_t) :: position

        position = self%position_
    end function
end module
