--  The structure of the MMS system has been created from the System
--  Requirements document following the pattern below:
--   * Components are packages.
--   * Sub-Components are child packages.
--   * Inputs and outputs are functions stored in child packages Comp.Input and
--     Comp.Output.
--   * Connections between inputs and outputs of various components are done
--     using renamings (or eventually expression functions when necessary).
--     Consistency of the component architecture is ensured by following the
--     rules below:
--      - An input package Comp.Subcomp.Input can only reference outputs of
--        siblings Comp.*.Output or inputs of parent Comp.Input.
--      - An output package Comp.Output can only reference its own inputs
--        Comp.Input or outputs of its children Comp.Subcomp.Output.
--      - In subcomponents, inputs and outputs are grouped within sections
--        with a header specifying to which component(s) they are linked.

package MMS is
end MMS;
