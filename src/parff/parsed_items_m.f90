module parff_parsed_items_m
    use parff_parsed_item_m, only: parsed_item_t
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_items_t

    type, extends(parsed_value_t) :: parsed_items_t
        private
        type(parsed_item_t), allocatable :: items_(:)
    contains
        private
        procedure, public :: items
    end type

    interface parsed_items_t
        module procedure constructor
    end interface
contains
    function constructor(items) result(parsed_items)
        type(parsed_item_t), intent(in) :: items(:)
        type(parsed_items_t) :: parsed_items

        allocate(parsed_items%items_, source = items)
    end function

    function items(self)
        class(parsed_items_t), intent(in) :: self
        type(parsed_item_t), allocatable :: items(:)

        allocate(items, source = self%items_)
    end function
end module
