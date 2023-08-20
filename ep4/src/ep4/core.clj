;; Definition of namespace -------------
(ns ep4.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as clj_str]
            [ep2.core :as ep2]))

;; Finite Automaton Functions --------------------

(defn GetAllPossibleSymbols 
  [grammar]
  (let [
    GetSymbol #(nth % 0)
    get_all_symbols (vec (map GetSymbol grammar))
    all_symbols_without_duplicate (distinct get_all_symbols)
  ]
    all_symbols_without_duplicate
  )
)

(defn GetNumberOfVariables
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")
    values (rule "value")

    all_symbols (GetAllPossibleSymbols grammar)
    number_of_symbols_in_values (count (filter #(contains? (set all_symbols) %) values))
  ]
    number_of_symbols_in_values
  )
)

(defn GetNumberOfTerminals
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")
    values (rule "value")

    number_of_termminals_in_values (count (filter #(contains? (set end_symbols) %) values))
  ]
    number_of_termminals_in_values
  )
)

(defn GetNumberEmptyVariables
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    values (rule "value")

    number_of_empties_variables (count (filter #(contains? (set ["ε"]) %) values))
  ]
    number_of_empties_variables
  )
)

(defn EmptyVariableValidation
  [grammar start_symbol end_symbols index]
  (let [
    rule (ep2/GetGramRule grammar index)
    symbol (rule "simbol")

    number_of_empties_variables (GetNumberEmptyVariables grammar start_symbol end_symbols index)

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

(defn IsThatGrammerInChomskyNormalForm
  [grammar start_symbol end_symbols]
  (let [
    grammar_len (count grammar)
    rules_position (range grammar_len)

    IsRuleWithInitialVariable #(InitialVariableValidation grammar start_symbol end_symbols %)
    IsEmptyVariableValid #(EmptyVariableValidation grammar start_symbol end_symbols %)
    IsOneTerminalValidation #(and (= (GetNumberOfTerminals grammar start_symbol end_symbols %) 1) (= (GetNumberOfVariables grammar start_symbol end_symbols %) 0))
    IsTwoVariablesValidation #(and (= (GetNumberOfTerminals grammar start_symbol end_symbols %) 0) (= (GetNumberOfVariables grammar start_symbol end_symbols %) 2))

    RuleChomskyNormalForm (fn [position] (and (or (IsOneTerminalValidation position) (IsTwoVariablesValidation position)) (IsEmptyVariableValid position) (IsRuleWithInitialVariable position)) )
    GrammarChomskyNormalFormArray (vec (map RuleChomskyNormalForm rules_position))
    IsGrammarInChomskyNormalForm (every? true? GrammarChomskyNormalFormArray)
  ]
    IsGrammarInChomskyNormalForm
  )
)

(defn RemoveInitalSymbolFromRightSide
  [grammar start_symbol end_symbols]
  (let
    [
      grammar_len (count grammar)
      rules_position (range grammar_len)

      InitialVariableRuleCheck #(InitialVariableValidation grammar start_symbol end_symbols %)
      InitialVariableGrammerCheck (every? true? (vec (map InitialVariableRuleCheck rules_position)))
    ]
    (if (not InitialVariableGrammerCheck)
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

(defn FindLastIndexOfSymbol
  ([symbol grammer] (FindLastIndexOfSymbol symbol grammer 0 0))
  ([symbol grammer actual_index last_position]
    (if (> actual_index (count grammer))
      last_position
      (if  (= ((ep2.core/GetGramRule grammer actual_index) "simbol") symbol)
        (FindLastIndexOfSymbol symbol grammer (+ actual_index 1) actual_index)
        (FindLastIndexOfSymbol symbol grammer (+ actual_index 1) last_position)
      )
    )
  )
)

(defn AddRuleInGrammer
  [grammer elements symbol]
  (if (empty? elements)
    grammer
    (let
      [
        last_symbol_position (FindLastIndexOfSymbol symbol grammer)
        element_to_add (get elements 0)

        [grammer_before grammer_after] (split-at (+ last_symbol_position 1) grammer)
        grammer_updated (vec (concat grammer_before [[symbol element_to_add]] grammer_after))
        elements_updated (subvec elements 1)
      ]
      (AddRuleInGrammer grammer_updated elements_updated symbol)
    )
  )
)

(defn GenerateAllCombinationsByRemovedElement 
  [elements symbol]
  (let
    [
      SymbolCounter (fn [elements] (count (filter #(= symbol %) elements)))
    ]
      (if (= (SymbolCounter elements) 0)
        []
        (if (and (= (count elements) 1) (= (get elements 0) symbol))
          [["ε"]]
          (let
            [
              number_of_elements (count elements)
              all_combination_sizes (range 1 number_of_elements)

              AddValueInStr (fn [string index] (str string "-" index))
              elements_with_number (map AddValueInStr elements (range number_of_elements))
              all_possible_values_with_number (map #(comb/combinations elements_with_number %) all_combination_sizes)

              RemoveNumberFromStr #(first (clj_str/split % #"-"))
              RemoveNumberFromRules #(map (fn [value] (map RemoveNumberFromStr value)) %)
              all_possible_values (map RemoveNumberFromRules all_possible_values_with_number)

              value_filter_function (fn [values index] (filter #(< (SymbolCounter %) index) values) )
              all_possible_values_filter (map value_filter_function all_possible_values all_combination_sizes)

              TransformElementInVec #(map vec %)
              all_possible_values_vec (vec (map #(vec (TransformElementInVec %)) all_possible_values_filter))

              FlatArray (fn [value] (if (= (count value) 1) (first value) value))
              all_possible_values_vec_flatten (vec (map FlatArray all_possible_values_vec))

              FlattenNested (fn [data]
                (mapcat #(if (vector? (first %)) % [%]) data))

              all_possible_values_vec_formated (vec (FlattenNested all_possible_values_vec_flatten))
            ]
            all_possible_values_vec_formated
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
                (if (= (get rule 1) ["ε"]) 
                  false
                  true
                )
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
            new_grammer (AddRuleInGrammer grammar values_to_tranform_filted current_symbol_to_apply)
          ]
          (ApplyEmptySymbolInGrammar new_grammer start_symbol end_symbols index (+ curr_symbol_index 1))
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
        all_symbols (vec (reverse (GetAllPossibleSymbols grammar)))
        number_of_symbols (count all_symbols)
      ]
      (if (>= index number_of_symbols)
        grammar
        (let
          [
            symbol_to_analyse (get all_symbols index) 
            SymbolGrammerFilter (
              fn [rule] 
              (if (= (get rule 0) symbol_to_analyse)
                true
                false
              ) 
            )

            grammer_filted (vec (filter SymbolGrammerFilter grammar))

            rule_elements (ep2.core/GetApplyRuleInElement grammar symbol_to_analyse)
            rules_positions (range (count rule_elements))

            element_empty_verification #(and
              (= (GetNumberOfTerminals grammer_filted start_symbol end_symbols %) 0)
              (= (GetNumberOfVariables grammer_filted start_symbol end_symbols %) 0)
              (> (GetNumberEmptyVariables grammer_filted start_symbol end_symbols %) 0)
            )

            rule_empty_verification (vec (map element_empty_verification rules_positions))
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

(defn PerformeChomskyNormalization
  [grammar start_symbol end_symbols]
  (let 
    [
      [
        grammar_initial_value_corrected
        start_symbol_initial_value_corrected
      ] (RemoveInitalSymbolFromRightSide grammar start_symbol end_symbols)

      grammar_without_empty_values (RemoveAllPossibleEmptyValues grammar_initial_value_corrected start_symbol_initial_value_corrected end_symbols)
    ]
    grammar_without_empty_values
  )
)


;; Main function ----------------------

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

  (def is_grammer_in_chomsky_normal_form (IsThatGrammerInChomskyNormalForm grammar start_symbol end_symbols))
  (if (not is_grammer_in_chomsky_normal_form)
    (println "Regra gramatical normalizada: " (PerformeChomskyNormalization grammar start_symbol end_symbols))
    (println "Regra gramatical jah esta normalizada")
  )

)