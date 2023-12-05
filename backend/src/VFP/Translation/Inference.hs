-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Translation.Inference where

import VFP.Translation.TreeBuilderOutput
import VFP.Translation.TreeBuilderInput

import VFP.UI.UIModel
import VFP.Inference.Elaboration (elaboration)
import VFP.Inference.Unification (unification)
import VFP.Inference.Zonking (zonking)

infere :: UntypedValue -> InferenceResult
infere untyped =
    let inputTree = buildInputTree untyped
        (elaboratedExpression, typeConstraints) = elaboration inputTree
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in case zonked of
        Left e -> Error e
        Right r -> Success $ buildOutputTree r
