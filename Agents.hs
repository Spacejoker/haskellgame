module Agents where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi

import Model 

initAgents :: IO[Agent]
initAgents = do
  agentSprite <- SDLi.load "image/agent.png"
  return [Agent "A" (Position 10 10) agentSprite, Agent "B" (Position 20 20) agentSprite]

