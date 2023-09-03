;; Definition of namespace -------------
(ns ep4.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as clj_str]
            [ep2.core :as ep2]))

;; --------------------------------------
;; General functions 
;; --------------------------------------

(defn GetAllPossibleSymbols 
  [grammar]
  (let [
    GetSymbol #(nth % 0)
    get_all_symbols (vec (map GetSymbol grammar))
    all_symbols_without_duplicate (distinct get_all_symbols)
  ]
    (vec all_symbols_without_duplicate)
  )
)

(defn GetNumberOfVariablesByIndex
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    elem (rule "value")

    all_symbols (GetAllPossibleSymbols grammar)
    number_of_variable_in_element (count (filter #(contains? (set all_symbols) %) elem))
  ]
    number_of_variable_in_element
  )
)

(defn GetNumberOfVariablesByElement
  [grammar start_symbol end_symbols elem]
  (let [
    all_symbols (GetAllPossibleSymbols grammar)
    number_of_variable_in_element (count (filter #(contains? (set all_symbols) %) elem))
  ]
    number_of_variable_in_element
  )
)

(defn GetNumberOfTerminalsByIndex
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    elem (rule "value")

    number_of_termminals_in_values (count (filter #(contains? (set end_symbols) %) elem))
  ]
    number_of_termminals_in_values
  )
)

(defn GetNumberOfTerminalsByElement
  [grammar start_symbol end_symbols elem]
  (let [
    number_of_termminals_in_values (count (filter #(contains? (set end_symbols) %) elem))
  ]
    number_of_termminals_in_values
  )
)

(defn GetNumberEmptyVariablesByIndex
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    elem (rule "value")

    number_of_empties_variables (count (filter #(contains? (set ["ε"]) %) elem))
  ]
    number_of_empties_variables
  )
)

(defn GetNumberEmptyVariablesByElement
  [grammar start_symbol end_symbols elem]
  (let [
    number_of_empties_variables (count (filter #(contains? (set ["ε"]) %) elem))
  ]
    number_of_empties_variables
  )
)

(defn EmptyVariableValidation
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")

    number_of_empties_variables (GetNumberEmptyVariablesByIndex grammar start_symbol end_symbols index)

    empty_variables_validation (or
      (= number_of_empties_variables 0)
      (and (= symbol start_symbol) (> number_of_empties_variables 0))
    )
  ]
    empty_variables_validation
  )
)

(defn InitialVariableValidation
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")
    values (rule "value")

    number_of_initial_variables (count (filter #(contains? (set [start_symbol]) %) values))
  ]
    (= number_of_initial_variables 0)
  )
)

;; Refazer isso depois com uma lógica melhor (pegar todas as posições que o simbolo aparece e retonar a última posição desse vetor de posição)
(defn FindLastIndexOfSymbol
  ([Grammar symbol] (FindLastIndexOfSymbol Grammar symbol 0 nil))
  ([Grammar symbol actual_index last_position]
    (if (> actual_index (count Grammar))
      last_position
      (if  (= ((ep2.core/GetGramRule Grammar actual_index) "simbol") symbol)
        (FindLastIndexOfSymbol Grammar symbol (+ actual_index 1) actual_index)
        (FindLastIndexOfSymbol Grammar symbol (+ actual_index 1) last_position)
      )
    )
  )
)

(defn FindFirstIndexOfSymbol
  [grammar symbol]
  (let 
    [
      all_positions (vec (range (count grammar)))
      
      MapFilter (
        fn [rule pos] 
        (if (= (get rule 0) symbol)
          pos
          nil
        )
      )

      all_position_that_appear_the_symbol (vec (map MapFilter grammar all_positions))

      all_position_that_appear_the_symbol_without_nil_values (vec (filter #(not (= % nil)) all_position_that_appear_the_symbol))
    ]
      (if (empty? all_position_that_appear_the_symbol_without_nil_values)
        nil
        (get all_position_that_appear_the_symbol_without_nil_values 0)
      )
  )
)

(defn AddRuleInGrammar
  [Grammar elements symbol]
  (if (empty? elements)
    Grammar
    (let
      [
        all_possible_symbol (vec (GetAllPossibleSymbols Grammar))
        symbol_exist (contains? (set all_possible_symbol) symbol)

        last_symbol_position (if symbol_exist
                                (FindLastIndexOfSymbol Grammar symbol)
                                (- (count Grammar) 1)
                                )
        element_to_add (get elements 0)

        [Grammar_before Grammar_after] (split-at (+ last_symbol_position 1) Grammar)
        Grammar_updated (vec (concat Grammar_before [[symbol element_to_add]] Grammar_after))
        elements_updated (subvec elements 1)
      ]
      (AddRuleInGrammar Grammar_updated elements_updated symbol)
    )
  )
)

;; -------------------------------------- 
;; Chomsky normal form verification 
;; --------------------------------------

(defn IsThatGrammarInChomskyNormalForm
  [grammar start_symbol end_symbols]
  (let [
    grammar_len (count grammar)
    rules_position (range grammar_len)

    IsRuleWithInitialVariable #(InitialVariableValidation grammar start_symbol end_symbols %)
    IsEmptyVariableValid #(EmptyVariableValidation grammar start_symbol end_symbols %)
    IsOneTerminalValidation #(and (= (GetNumberOfTerminalsByIndex grammar start_symbol end_symbols %) 1) (= (GetNumberOfVariablesByIndex grammar start_symbol end_symbols %) 0))
    IsTwoVariablesValidation #(and (= (GetNumberOfTerminalsByIndex grammar start_symbol end_symbols %) 0) (= (GetNumberOfVariablesByIndex grammar start_symbol end_symbols %) 2))

    RuleChomskyNormalForm (fn [position] (and (or (IsOneTerminalValidation position) (IsTwoVariablesValidation position)) (IsEmptyVariableValid position) (IsRuleWithInitialVariable position)) )
    GrammarChomskyNormalFormArray (vec (map RuleChomskyNormalForm rules_position))
    IsGrammarInChomskyNormalForm (every? true? GrammarChomskyNormalFormArray)
  ]
    IsGrammarInChomskyNormalForm
  )
)

;; -------------------------------------- 
;; Remove Initial Symbols from right side 
;; --------------------------------------

(defn RemoveInitalSymbolFromRightSide
  [grammar start_symbol end_symbols]
  (let
    [
      grammar_len (count grammar)
      rules_position (range grammar_len)

      InitialVariableRuleCheck #(InitialVariableValidation grammar start_symbol end_symbols %)
      InitialVariableGrammarCheck (every? true? (vec (map InitialVariableRuleCheck rules_position)))
    ]
    (if (not InitialVariableGrammarCheck)
      (let
        [
          new_start_symbol "S0"
          new_grammar (vec (concat [["S0" ["S"]]] grammar))
        ]
        [new_grammar new_start_symbol]
      )
      [grammar start_symbol]
    )

  )
)

;; --------------------------------------
;; Remove Empties values
;; --------------------------------------

(defn GenerateAllCombinationsByRemovedElement 
  [elements symbol]
  (let
    [
      SymbolCounter (fn [elem] (count (filter #(= symbol %) elem)))
      total_symbol_element (SymbolCounter elements)
    ]
      (if (= total_symbol_element 0)
        []
        (if (and (= (count elements) 1) (= (get elements 0) symbol))
          [["ε"]]
          (let
            [
              number_of_elements (count elements)
              AddValueInStr (fn [string index] (str string "-" index))
              elements_with_number (map AddValueInStr elements (range number_of_elements))

              all_combination_sizes (range 1 number_of_elements)
              all_possible_values_with_number (map #(comb/combinations elements_with_number %) all_combination_sizes)

              RemoveNumberFromStr #(first (clj_str/split % #"-"))
              RemoveNumberFromRules #(map (fn [value] (map RemoveNumberFromStr value)) %)
              all_possible_values (map RemoveNumberFromRules all_possible_values_with_number)

              accept_sizes_threshold (- (count elements) total_symbol_element)
              ValueFilterFunction (fn [values size] (filter #(and
                (= (SymbolCounter %) (- size (- number_of_elements total_symbol_element) ) )
                (>= (count %) accept_sizes_threshold) )
                values)
              )
              all_possible_values_filter (map ValueFilterFunction all_possible_values all_combination_sizes)

              TransformElementInVec #(map vec %)
              all_possible_values_vec (vec (map #(vec (TransformElementInVec %)) all_possible_values_filter))

              FlatArray (fn [value] (if (= (count value) 1) (first value) value))
              all_possible_values_vec_flatten (vec (map FlatArray all_possible_values_vec))

              FlattenNested (fn [data]
                (mapcat #(if (vector? (first %)) % [%]) data))

              all_possible_values_vec_formated (vec (FlattenNested all_possible_values_vec_flatten))

              all_possible_values_vec_formated_without_empty_values (vec (filter #(not (= % [])) all_possible_values_vec_formated) )
            ]
            all_possible_values_vec_formated_without_empty_values
          )
        )
      )

  )
)

(defn ApplyEmptySymbolInGrammar
  ([grammar start_symbol end_symbols index] (ApplyEmptySymbolInGrammar grammar start_symbol end_symbols index 0))
  ([grammar start_symbol end_symbols index curr_symbol_index]
    (let
      [
        all_possible_symbols (reverse (GetAllPossibleSymbols grammar))
        GetSymbol #(nth all_possible_symbols %)
        symbol_to_analyse (GetSymbol index)

        number_list_symbols_to_apply (vec (range (+ index 1) (count all_possible_symbols)))
        symbols_to_apply (vec (map GetSymbol number_list_symbols_to_apply))
      ]
      (if (> curr_symbol_index (count symbols_to_apply))
        (let
          [
            EmptyFilter (
              fn [rule] 
              (if (not (= (get rule 0) symbol_to_analyse)) 
                true
                (not (= (get rule 1) ["ε"]))
              ) 
            )
            final_grammar (vec (filter EmptyFilter grammar))
          ]
          final_grammar
        )
        (let
          [
            current_symbol_to_apply (get symbols_to_apply curr_symbol_index)
            values_to_add (ep2.core/GetApplyRuleInElement grammar current_symbol_to_apply)
            values_to_tranform (vec (map #(GenerateAllCombinationsByRemovedElement % symbol_to_analyse) values_to_add))
            FlattenNested (fn [data]
              (mapcat #(if (vector? (first %)) % [%]) data))
            values_to_tranform_flatted (vec (FlattenNested values_to_tranform))
            values_to_tranform_filted (vec (filter #(not (empty? %)) values_to_tranform_flatted))
            new_Grammar (AddRuleInGrammar grammar values_to_tranform_filted current_symbol_to_apply)
          ]
          (ApplyEmptySymbolInGrammar new_Grammar start_symbol end_symbols index (+ curr_symbol_index 1))
        )
      )
    )
  )
)

(defn RemoveAllPossibleEmptyValues
  ([grammar start_symbol end_symbols] (RemoveAllPossibleEmptyValues grammar start_symbol end_symbols 0))
  ([grammar start_symbol end_symbols index]
    (let
      [
        all_symbols_reversed (vec (reverse (GetAllPossibleSymbols grammar)))
        number_of_symbols (count all_symbols_reversed)
      ]
      (if (>= index number_of_symbols)
        grammar
        (let
          [
            symbol_to_analyse (get all_symbols_reversed index) 
            grammar_filted (vec (filter #(= (get % 0) symbol_to_analyse) grammar))

            rule_elements (ep2.core/GetApplyRuleInElement grammar symbol_to_analyse)
            rules_positions (range (count rule_elements))

            ElementEmptyVerification #(and
              (= (GetNumberOfTerminalsByIndex grammar_filted start_symbol end_symbols %) 0)
              (= (GetNumberOfVariablesByIndex grammar_filted start_symbol end_symbols %) 0)
              (> (GetNumberEmptyVariablesByIndex grammar_filted start_symbol end_symbols %) 0)
            )

            rule_empty_verification (vec (map ElementEmptyVerification rules_positions))
            is_to_apply_empty_elimination (not (every? false? rule_empty_verification))
          ]
            (if is_to_apply_empty_elimination
              (let
                [
                  new_grammar (ApplyEmptySymbolInGrammar grammar start_symbol end_symbols index)
                ]
                (RemoveAllPossibleEmptyValues new_grammar start_symbol end_symbols (+ index 1))
              )
              (RemoveAllPossibleEmptyValues grammar start_symbol end_symbols (+ index 1))
            )
        )
      )
    )
  )
)

;; -------------------------------------- 
;; Remove redundant values 
;; --------------------------------------

(defn RemoveRedundantValues
  [grammar]
  (let
    [
      RepeatedValuesMap (
        fn [rule pos]
        (let 
          [
            rule_symbol (get rule 0)
            rule_value (get rule 1)
            grammar_values_before_position (vec (map #(get grammar %) (range pos) ))
            grammar_values_before_position_per_symbol (ep2.core/GetApplyRuleInElement grammar_values_before_position rule_symbol)
          ]
            (if (not (contains? (set grammar_values_before_position_per_symbol) rule_value))
              rule
              [rule_symbol [rule_symbol]]
            )
        )
      )
      
      grammar_repeated_values_filted (vec (map RepeatedValuesMap grammar (range (count grammar)) ))

      SymbolEqualValueFilter (
        fn [rule]
        (not (= [(get rule 0)] (get rule 1)))
      )

      grammar_equal_value_filted (vec (filter SymbolEqualValueFilter grammar_repeated_values_filted))

    ]
    grammar_equal_value_filted
  )
)

;; -------------------------------------- 
;; Remove unit values 
;; --------------------------------------

(defn RemoveAllUnitValues
  ([grammar start_symbol end_symbols] (RemoveAllUnitValues grammar start_symbol end_symbols 0 grammar))
  ([grammar start_symbol end_symbols index initial_grammar]
    (let
      [
        all_possible_symbols (vec (GetAllPossibleSymbols grammar))
        number_of_symbols (count all_possible_symbols)
      ]
      (if (>= index number_of_symbols)
        (if (not (= grammar initial_grammar))
          (RemoveAllUnitValues grammar start_symbol end_symbols 0 grammar)
          grammar
        )
        (let
          [
            symbol_to_analyse (get all_possible_symbols index)
            values_from_symbol (vec (ep2.core/GetApplyRuleInElement grammar symbol_to_analyse))

            UnitVariableTranformation (
              fn [element]
              (let
                [
                  number_of_variable_in_element (GetNumberOfVariablesByElement grammar start_symbol end_symbols element)
                  number_of_terminal_in_element (GetNumberOfTerminalsByElement grammar start_symbol end_symbols element)
                  number_of_empty_in_element (GetNumberEmptyVariablesByElement grammar start_symbol end_symbols element)
                ]
                (if (and (= number_of_variable_in_element 1) (= number_of_terminal_in_element 0) (= number_of_empty_in_element 0))
                  (vec (ep2.core/GetApplyRuleInElement grammar (get element 0)))
                  element
                )
              )
            )

            new_elems_from_symbol_without_unit_value (vec (map UnitVariableTranformation values_from_symbol))
            FlattenNested (fn [data]
              (mapcat #(if (vector? (first %)) % [%]) data))
            new_elems_from_symbol_without_unit_value_flatted (vec (FlattenNested new_elems_from_symbol_without_unit_value))
            
            grammar_with_new_values (AddRuleInGrammar grammar new_elems_from_symbol_without_unit_value_flatted symbol_to_analyse)

            OldElemMapFilter (
              fn [rule actual_index]
              (let
                [
                  initial_index_from_symbol (FindFirstIndexOfSymbol grammar symbol_to_analyse)
                  last_index_from_symbol (FindLastIndexOfSymbol grammar symbol_to_analyse)
                ]
                (if (and (>= actual_index initial_index_from_symbol) (<= actual_index last_index_from_symbol))
                  nil
                  rule
                )
              )
            )

            grammar_without_old_elem (vec (map OldElemMapFilter grammar_with_new_values (vec (range (count grammar_with_new_values))) ))

            grammar_without_old_elem_and_nil (vec (filter #(not (= % nil)) grammar_without_old_elem))

            grammar_without_old_elem_and_nil_and_repeated_values (RemoveRedundantValues grammar_without_old_elem_and_nil)
          ]
            (RemoveAllUnitValues grammar_without_old_elem_and_nil_and_repeated_values start_symbol end_symbols (+ index 1) initial_grammar)
        )
      )
    )
  )
)

;; -------------------------------------- 
;; Create new variables to grammar 
;; --------------------------------------


(defn GrammarCorrectionVerificationByIndex
  [grammar start_symbol end_symbols index ver_case]
  (let
    [
      current_rule (get grammar index)
      current_element (get current_rule 1)
      element_size (count current_element)

      total_number_of_terminals (GetNumberOfTerminalsByElement grammar start_symbol end_symbols current_element)
      total_number_of_variables (GetNumberOfVariablesByElement grammar start_symbol end_symbols current_element)
      total_number_of_empties (GetNumberEmptyVariablesByElement grammar start_symbol end_symbols current_element) 
    ]
      (if (or (<= element_size 1) (and (= element_size 2) (= total_number_of_variables 2)))
        false
        (let
          [
            final_elements (vec (subvec current_element (- element_size 2)))
            local_number_of_variables (GetNumberOfVariablesByElement grammar start_symbol end_symbols final_elements)
            local_number_of_terminals (GetNumberOfTerminalsByElement grammar start_symbol end_symbols final_elements)
            local_number_of_empties (GetNumberEmptyVariablesByElement grammar start_symbol end_symbols final_elements)
          ] 
          (if (= ver_case 0)
            (and (= local_number_of_terminals 1) (= local_number_of_variables 1))
            (if (= ver_case 1)
              (= local_number_of_variables 2)
              (if (= ver_case 2)
                (= local_number_of_terminals 2)
                false
              )
            )
          )
        )
      )
  )
)

(defn GrammarCorrectionVerificationByElement
  [grammar start_symbol end_symbols elem ver_case]
  (let
    [
      current_element elem
      element_size (count current_element)

      total_number_of_terminals (GetNumberOfTerminalsByElement grammar start_symbol end_symbols current_element)
      total_number_of_variables (GetNumberOfVariablesByElement grammar start_symbol end_symbols current_element)
      total_number_of_empties (GetNumberEmptyVariablesByElement grammar start_symbol end_symbols current_element) 
    ]
      (if (or (<= element_size 1) (and (= element_size 2) (= total_number_of_variables 2)))
        false
        (let
          [
            final_elements (vec (subvec current_element (- element_size 2)))
            local_number_of_variables (GetNumberOfVariablesByElement grammar start_symbol end_symbols final_elements)
            local_number_of_terminals (GetNumberOfTerminalsByElement grammar start_symbol end_symbols final_elements)
            local_number_of_empties (GetNumberEmptyVariablesByElement grammar start_symbol end_symbols final_elements)
          ] 
          (if (= ver_case 0)
            (and (= local_number_of_terminals 1) (= local_number_of_variables 1))
            (if (= ver_case 1)
              (= local_number_of_variables 2)
              (if (= ver_case 2)
                (= local_number_of_terminals 2)
                false
              )
            )
          )
        )
      )
  )
)

;; tem uma parte do código repetido aqui, melhorar isso depois
(defn GetSymbolToSwap
  [grammar elem]
  (let
    [
      all_possible_symbols (GetAllPossibleSymbols grammar)
      filter_function #(= (count (ep2.core/GetApplyRuleInElement grammar %)) 1)
      all_possible_symbols_with_one_value (vec (filter filter_function all_possible_symbols))
    ]
    (if (>= (count all_possible_symbols_with_one_value) 1)
      (let
        [
          one_element_filter #(= (ep2.core/GetApplyRuleInElement grammar %) [elem])
          all_possible_symbol_with_element (vec (filter one_element_filter all_possible_symbols_with_one_value))
        ]
        (if (>= (count all_possible_symbol_with_element) 1)
          [grammar (get all_possible_symbol_with_element 0)]
          (let
            [
              last_symbol (last all_possible_symbols)
              new_symbol (str last_symbol 1)
              new_grammar (ep4.core/AddRuleInGrammar grammar [elem] new_symbol)
            ]
            [new_grammar new_symbol]
          )
        )
      )
      (let
        [
          last_symbol (last all_possible_symbols)
          new_symbol (str last_symbol 1)
          new_grammar (ep4.core/AddRuleInGrammar grammar [elem] new_symbol)
        ]
        [new_grammar new_symbol]
      )
    )
  )
)

(defn TwoVariablesCorrection
  [grammar start_symbol end_symbols index]
  (let
    [
      current_rule (get grammar index)
      current_symbol (get current_rule 0)
      current_element (get current_rule 1)

      GetLastTwoElements #(vec (subvec % (- (count %) 2)))
      last_two_values_curr_elem (GetLastTwoElements current_element)
      [new_grammar symbol_to_swap] (GetSymbolToSwap grammar last_two_values_curr_elem)

      SwapLastTwoValuesInElement #(vec (concat (get (split-at (- (count %) 2) %) 0) [symbol_to_swap]))

      SwapCondition #(and 
                      (GrammarCorrectionVerificationByElement grammar start_symbol end_symbols % 1) 
                      (= (GetLastTwoElements %) last_two_values_curr_elem)
                    )
      AddNewElementInRule (fn [rule] 
                  (if (SwapCondition (get rule 1))
                  [(get rule 0) (SwapLastTwoValuesInElement (get rule 1))]
                  rule
      ))
      corrected_grammar (vec (map AddNewElementInRule new_grammar))
    ]
    corrected_grammar
  )
)

(defn TerminalVariableCorrection
  [grammar start_symbol end_symbols index]
  (let
    [
      current_rule (get grammar index)
      current_symbol (get current_rule 0)
      current_element (get current_rule 1)

      GetLastTwoElements (fn [elem](vec (subvec elem (- (count elem) 2))))
      GetTerminalElements (fn [elem] (get (vec (filter #(contains? (set end_symbols) %) (GetLastTwoElements elem) ) ) 0))
      terminal_value_curr_elem (GetTerminalElements current_element)
      [new_grammar symbol_to_swap] (GetSymbolToSwap grammar [terminal_value_curr_elem])

      SwapTerminalValuesInElement (fn [elem]
        (let
          [
            last_two_elem (GetLastTwoElements elem)
          ]
          (if (= (get last_two_elem 0) terminal_value_curr_elem)
            (vec (concat (get (split-at (- (count elem) 2) elem) 0) [symbol_to_swap (get last_two_elem 1)]))
            (vec (concat (get (split-at (- (count elem) 2) elem) 0) [(get last_two_elem 0) symbol_to_swap]))
          )
        )
      )


      SwapCondition #(and 
                      (GrammarCorrectionVerificationByElement grammar start_symbol end_symbols % 0) 
                      (= (GetTerminalElements %) terminal_value_curr_elem)
                    )
      AddNewElementInRule (fn [rule] 
                  (if (SwapCondition (get rule 1))
                  [(get rule 0) (SwapTerminalValuesInElement (get rule 1))]
                  rule
      ))
      corrected_grammar (vec (map AddNewElementInRule new_grammar))
    ]
    corrected_grammar
  )
)

(defn TwoTerminalsCorrection
  [grammar start_symbol end_symbols index]
  (let
    [
      current_rule (get grammar index)
      current_symbol (get current_rule 0)
      current_element (get current_rule 1)

      GetLastTwoElements (fn [elem](vec (subvec elem (- (count elem) 2))))
      terminals_value_curr_elem (GetLastTwoElements current_element)
      [inter_grammar symbol0_to_swap] (GetSymbolToSwap grammar [(get terminals_value_curr_elem 0)])
      [new_grammar symbol1_to_swap] (GetSymbolToSwap inter_grammar [(get terminals_value_curr_elem 1)])

      SwapTerminalValuesInElement (fn [elem]
        (vec (concat (get (split-at (- (count elem) 2) elem) 0) [symbol0_to_swap symbol1_to_swap]))
      )

      SwapCondition #(and 
                      (GrammarCorrectionVerificationByElement grammar start_symbol end_symbols % 2) 
                      (= (GetLastTwoElements %) terminals_value_curr_elem)
                    )
      AddNewElementInRule (fn [rule] 
                  (if (SwapCondition (get rule 1))
                  [(get rule 0) (SwapTerminalValuesInElement (get rule 1))]
                  rule
      ))
      corrected_grammar (vec (map AddNewElementInRule new_grammar))
    ]
    corrected_grammar
  )
)

;; Fazer isso executar em loop até que todos os valores estares com duas variaveis ou um terminal 
;; (ignorar variaveis unitárias ou valores vazios, isso deveria ser trabalho das funções anteriores resolverem isso)
(defn AddNewVariablesToGrammar
  ([grammar start_symbol end_symbols] (AddNewVariablesToGrammar grammar start_symbol end_symbols 0 grammar))
  ([grammar start_symbol end_symbols index inital_grammar]
    (let
      [
        num_of_rules (count grammar)
        is_to_continue (>= num_of_rules index)
      ]
        (if (not is_to_continue)
          (if (not (= grammar inital_grammar))
            (AddNewVariablesToGrammar grammar start_symbol end_symbols 0 grammar)
            grammar
          )
          (let
            [
              current_rule (get grammar index)
              current_symbol (get current_rule 0)
              current_element (get current_rule 1)

              is_to_apply_terminal_variable_correction (GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols index 0)
              is_to_apply_double_variable_correction (GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols index 1)
              is_to_apply_double_terminal_correction (GrammarCorrectionVerificationByIndex grammar start_symbol end_symbols index 2)
            ]
              (if is_to_apply_terminal_variable_correction
                (AddNewVariablesToGrammar (TerminalVariableCorrection grammar start_symbol end_symbols index) start_symbol end_symbols (+ index 1) inital_grammar)
                (if is_to_apply_double_variable_correction
                  (AddNewVariablesToGrammar (TwoVariablesCorrection grammar start_symbol end_symbols index) start_symbol end_symbols (+ index 1) inital_grammar)
                  (if is_to_apply_double_terminal_correction
                    (AddNewVariablesToGrammar (TwoTerminalsCorrection grammar start_symbol end_symbols index) start_symbol end_symbols (+ index 1) inital_grammar)
                    (AddNewVariablesToGrammar grammar start_symbol end_symbols (+ index 1) inital_grammar)
                  )
                )
              )
          )
        )
    )
  )
)

;; --------------------------------------
;; Chomsky Normalization
;; --------------------------------------

(defn PerformeChomskyNormalization
  [grammar start_symbol end_symbols]
  (let 
    [
      [
        grammar_initial_value_corrected
        start_symbol_initial_value_corrected
      ] (RemoveInitalSymbolFromRightSide grammar start_symbol end_symbols)

      grammar_without_empty_values (RemoveAllPossibleEmptyValues grammar_initial_value_corrected start_symbol_initial_value_corrected end_symbols)

      grammar_filted (RemoveRedundantValues grammar_without_empty_values)

      grammar_without_unit_values (RemoveAllUnitValues grammar_filted start_symbol_initial_value_corrected end_symbols)

      grammar_with_new_swap_values (AddNewVariablesToGrammar grammar_without_unit_values start_symbol_initial_value_corrected end_symbols)
    ]
    [
      (IsThatGrammarInChomskyNormalForm grammar_with_new_swap_values start_symbol_initial_value_corrected end_symbols)
      grammar_with_new_swap_values
    ]
  )
)

;; --------------------------------------
;; Main function 
;; --------------------------------------

(defn -main []
  (def grammar [["S" ["A", "S", "A"]]
                   ["S" ["a", "B"]]
                   ["A" ["B"]]
                   ["A" ["S"]]
                   ["B" ["b"]]
                   ["B" ["ε"]]])

  (def start_symbol "S")
  (def end_symbols ["a" "b"])

  (println "Regras gramaticais: " grammar)
  (println "Simbolos iniciais: " start_symbol)
  (println "Simbolos finais: " end_symbols)

  (def is_Grammar_in_chomsky_normal_form (IsThatGrammarInChomskyNormalForm grammar start_symbol end_symbols))
  (if (not is_Grammar_in_chomsky_normal_form)
    (println "Regra gramatical normalizada: " (get (PerformeChomskyNormalization grammar start_symbol end_symbols) 1))
    (println "Regra gramatical jah esta normalizada")
  )
)