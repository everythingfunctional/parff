module parff_parsed_item_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_item_t

    type :: parsed_item_t
        private
        class(parsed_value_t), allocatable :: item_
    contains
        private
        procedure, public :: item
    end type

    interface parsed_item_t
        module procedure constructor
    end interface
contains
    function constructor(item) result(parsed_item)
        class(parsed_value_t), intent(in) :: item
        type(parsed_item_t) :: parsed_item

        allocate(parsed_item%item_, source = item)
    end function

    function item(self)
        class(parsed_item_t), intent(in) :: self
        class(parsed_value_t), allocatable :: item

        allocate(item, source = self%item_)
    end function
end module
